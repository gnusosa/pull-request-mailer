{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

{-# LANGUAGE NamedFieldPuns, DeriveGeneric, DeriveDataTypeable #-}

module Github.PullRequests.Mailer where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON,
                   fromJSON, json')
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Typeable
import GHC.Generics
import GitHub.Auth
import GitHub.Data.PullRequests
import GitHub.Data.Definitions
import qualified GitHub.Endpoints.Issues.Comments as GH
import GitHub.Endpoints.PullRequests hiding (Error)
import qualified GitHub.Endpoints.PullRequests as GH
import System.Command (cmd, Stdout(..), Exit(..))
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)

import Github.PullRequests.Mailer.MsgId


-- | Terminates the program with exit code 1 and the given error message.
die :: (MonadError e m, Control.Monad.Error.Error e) => String -> m a
die = throwError . strMsg


-- | Logs a given message.
logInfo :: String -> IO ()
logInfo = hPutStrLn stderr


-- | Newtype around Github `Error`s so that we can throw them as exceptions.
data GithubError = GithubError String GH.Error
  deriving (Show, Typeable)

instance Exception GithubError

-- | Throws a Github Error as an Exception, adding a description of what
-- what action failed.
throwGithub :: String -> GH.Error -> IO a
throwGithub msg e = throwIO $ GithubError msg e


-- | If the value is 'Left', throw an error using `throwGithub` using the
-- supplied error message. Otherwise return the 'Right' part.
rightOrThrowGithub :: String -> Either GH.Error a -> IO a
rightOrThrowGithub msg = either (throwGithub msg) return


-- | Read-replace-write the contents of a file.
replaceTextInFile :: FilePath -> (Text -> Text) -> IO ()
replaceTextInFile path f = T.readFile path >>= T.writeFile path . f


-- | Remove leading and trailing white space from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack


-- | Identifies a pull request.
data PRID = PRID
  { pridOwner    :: Name Owner
  , pridRepoName :: Name Repo
  , pridNumber   :: IssueNumber
  } deriving (Eq, Ord, Show)


-- | Downloads a pull request from Github. Dies on error.
downloadPullRequest :: Maybe Auth -> PRID -> IO PullRequest
downloadPullRequest auth PRID{ pridOwner, pridRepoName, pridNumber } = do
  GH.pullRequest' auth pridOwner pridRepoName pridNumber >>=
    rightOrThrowGithub "Error getting pull request"


-- | Information about an email thread.
data ThreadInfo = ThreadInfo
  { tiMsgId     :: MsgId -- ^ the email message ID
  , tiIteration :: Int   -- ^ patch iteration (`reroll-count` in Git),
                         --   starting at 1
  } deriving (Eq, Ord, Show, Generic)

threadInfoJsonOptions :: JSON.Options
threadInfoJsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = \s -> case stripPrefix "ti" s of
      Just (x:xs) -> toLower x : xs
      Just xs     -> xs
      Nothing     -> error "threadInfoJsonOptions: bad prefix"
  }

instance FromJSON ThreadInfo where
  parseJSON = genericParseJSON threadInfoJsonOptions
instance ToJSON ThreadInfo where
  toJSON = genericToJSON threadInfoJsonOptions


-- | The string in a pull request comment that directly precedes a JSON
-- representation of `ThreadInfo`.
_THREAD_INFO_JSON_HEADER :: String
_THREAD_INFO_JSON_HEADER = "pull-request-mailer-data "



-- | Parses a Github issue comment body into a `ThreadInfo`, if it contains
-- one.
parseThreadInfo :: T.Text -> Maybe ThreadInfo
parseThreadInfo body = do
  let dataHeader = T.pack _THREAD_INFO_JSON_HEADER
      -- Find where the data starts.
      rest = T.encodeUtf8 . T.drop (T.length dataHeader)
             . last . T.splitOn dataHeader $ body

  -- Parse what follows as `ThreadInfo`, drop everything behind.
  case fromJSON <$> A.parseOnly json' rest of
    Right (JSON.Success threadInfo) -> Just threadInfo
    _                               -> Nothing


-- | Get the most recent `ThreadInfo` contained in a pull request's comments.
getMostRecentThreadInfo :: Maybe Auth -- ^ Github authentication
                        -> PRID             -- ^ from wich PR to get the info
                        -> IO (Maybe ThreadInfo)
getMostRecentThreadInfo auth PRID{ pridOwner, pridRepoName, pridNumber } =
  liftM (V.last . (V.cons Nothing) . V.map (parseThreadInfo . issueCommentBody)) .
  rightOrThrowGithub "Failed to get pull request comments" =<<
  GH.comments' auth pridOwner pridRepoName pridNumber


-- | Converts a pull request to a patch series using `git format-patch`.
sendPatchSeries :: Text              -- ^ recipient email address
                -> Maybe Text        -- ^ reply-to address
                -> Maybe ThreadInfo    -- ^ thread to reply to
                                       --   (previous iteration of the PR)
                -> Maybe Text        -- ^ post-checkout hook program
                -> PullRequest -- ^ the pull request to convert
                -> IO ThreadInfo
sendPatchSeries recipient replyTo prevThreadInfo checkoutHookCmd
  PullRequest
    { pullRequestHtmlUrl = url
    , pullRequestUser = prOwner
    , pullRequestTitle = title
    , pullRequestBody = body
    , pullRequestHead = PullRequestCommit
        { pullRequestCommitRef = tipBranch
        , pullRequestCommitRepo = Just Repo
            { repoName = tipRepoName
            , repoOwner = tipRepoOwner
            }
        }
    , pullRequestBase = PullRequestCommit
        { pullRequestCommitRef = baseBranch
        , pullRequestCommitRepo = Just Repo
            { repoName = baseRepoName
            , repoOwner = SimpleOwner {
                simpleOwnerLogin = baseOwnerLogin
                                      }
            }
        }
    } = do

  withSystemTempDirectory "pull-request-mailer" $ \tmpDir -> do

    -- Clone the base.
    let uri = show baseRepoOwner ++ "/" ++ show baseRepoName
    logInfo $ "Cloning " ++ uri
    () <- cmd ("git clone git://github.com/" ++ uri) "-b" (show baseBranch) tmpDir
    -- ^ We don't use --depth 1 here because git will send the whole history
    -- as patches if it cannot see a common base between our two commits.

    setCurrentDirectory tmpDir

    -- Add the pull request tipBranch as a remote.
    let uriR = show tipRepoOwner ++ "/" ++ show tipRepoName
    logInfo $ "Adding remote " ++ uriR
    () <- cmd ("git remote add pullrequest git://github.com/" ++ uriR)
    logInfo $ "Fetching from the remote: " ++ show tipBranch
    -- We would prefer to do `git fetch pullrequest tipBranch` here, but
    -- in git < 1.8.4 this doesn't make pullrequest/tipBranch available,
    -- so we have to fetch the whole remote in order to support this version.
    () <- cmd "git fetch pullrequest"

    () <- cmd "git branch -rv"
    () <- cmd "git remote -v"

    -- Run the post-checkout hook command if given.
    for_ checkoutHookCmd $ \hookCmd -> do
      logInfo $ "Running " ++ (show hookCmd)
      Exit code <- cmd (show hookCmd)
      when (code /= ExitSuccess) $ die "Post checkout hook failed. Aborting."

    let _PATCH_DIR_NAME = "patch-dir"
        patchVersionPrefix = case prevThreadInfo of
          Just (ThreadInfo _ lastN) -> "v" ++ show (lastN + 1) ++ "-"
          Nothing                   -> ""
        coverLetterPath = _PATCH_DIR_NAME
                          </> patchVersionPrefix ++ "0000-cover-letter.patch"

    -- Create the patch series.
    () <- cmd "git format-patch"
              "--cover-letter"
              (["--subject-prefix=PR PATCH " ++ show baseBranch]
               ++ case prevThreadInfo of
                    Nothing                       -> []
                    Just (ThreadInfo msgId lastN) ->
                      -- `--in-reply-to` allows email header injection.
                      -- `msgId` being of type MsgId makes it safe to use here
                      -- as it carries the invariant of being a valid msg-id
                      -- /only/.
                      [ "--in-reply-to=" ++ fromMsgId msgId
                      , "--reroll-count=" ++ show (lastN + 1)
                      ]
              )
              [ "--add-header=Reply-To: " ++ (show addr) | Just addr <- [replyTo] ]
              "--output-directory" _PATCH_DIR_NAME
              "--thread=shallow"
              ("origin/" ++ show baseBranch ++ "..pullrequest/" ++ show tipBranch)

    -- Get the Message-Id git format-patch assgined.
    msgId <- mkMsgId =<<
               fromMaybe (error "format-patch failed to create Message-Id")
             . fmap T.unpack
             . listToMaybe
             . mapMaybe (T.stripPrefix $ T.pack "Message-Id: ")
             . T.lines
             <$> T.readFile coverLetterPath

    -- Fill out the cover letter.
    Stdout gitUserEmailOutput <- cmd "git config --get user.email"
    let seriesSubmitter = strip gitUserEmailOutput
    let prUser = show prOwner
    let note = "******** NOTE: pull-request-mailer ********\n\
               \* User '" ++ prUser ++ "' has submitted a pull request on\n\
               \*   " ++ show url ++ "\n\
               \* \n\
               \* It has been converted to a patch series by \n\
               \*   " ++ seriesSubmitter ++ "\n\
               \***************************************************\n\
               \\n"
    replaceTextInFile coverLetterPath
      $ T.replace (T.pack "*** SUBJECT HERE ***") title
      . T.replace (T.pack "*** BLURB HERE ***") (T.pack $ note ++ show body)

    -- Send the email.
    () <- cmd "git send-email"
              "--no-thread" -- we do threading with `format-patch` above
              "--confirm=never" -- be non-interactive
              ["--to=" ++ (show recipient), "--from=" ++ seriesSubmitter]
              _PATCH_DIR_NAME

    return $ ThreadInfo msgId (maybe 1 (succ . tiIteration) prevThreadInfo)


-- | Creates a comment in the pull request stating that the PR was converted
-- to an email thread using this program and that users shall not reply to the
-- PR, but continue the discussion at the given location instead.
-- It states the used message ID of the first email in a parsable format so
-- that subsequent invocations can be sent as replies to the previous threads.
--
-- This way we can achieve threading as given in the example in
-- `man git send-email`.
--
-- [PATCH branch 0/2] Here is what I did...
--   [PATCH branch 1/2] Clean up and tests
--   [PATCH branch 2/2] Implementation
--   [PATCH branch v2 0/3] Here is a reroll
--     [PATCH branch v2 1/3] Clean up
--     [PATCH branch v2 2/3] New tests
--     [PATCH branch v2 3/3] Implementation
--
-- It also includes the number of times the pull request was turned into
-- a thread so that force-pushed improvements to the PR can be sent with
-- the correct `--reroll-count` option to `git format-patch`.
-- The first iteration (original patch) should be passed as reroll-count=1.
postMailerInfoComment :: Auth -- ^ Github authentication
                      -> PRID       -- ^ on which PR to comment
                      -> String     -- ^ discussion location
                      -> ThreadInfo -- ^ information to post for subsequent
                                    --   invocations
                      -> IO ()
postMailerInfoComment auth prid discussionLocation threadInfo = do
  let PRID{ pridOwner, pridRepoName, pridNumber } = prid
  -- Note: The message ID in `threadInfo` contains "<" and ">".
  --       aeson's `encode` turns them into not-so-nice "\u003c" and "\u003e"
  --       for silly reasons, see:
  --         https://github.com/bos/aeson/issues/180#issuecomment-54386449
  let dat = BSL8.unpack . JSON.encode $ threadInfo
  let msg = T.pack $
        "`AUTOGENERATED MESSAGE by pull-request-mailer`\n\
        \This pull request has been converted to an email thread on\
        \ " ++ discussionLocation ++ ". Discussion continues there.\n\
        \\n\
        \**Please do not post comments to this pull request.**\
        \ The email thread will not get notified.\n\
        \\n\
        \<!-- " ++ _THREAD_INFO_JSON_HEADER ++ dat ++ " -->"

  GH.createComment auth pridOwner pridRepoName pridNumber msg >>=
    rightOrThrowGithub "Failed to create comment" >>
    return ()


-- | Converts a detailed pull request into a 'PRID'.
detailedPullRequestToPRID :: PullRequest -> PRID
detailedPullRequestToPRID dpr =
  PRID (simpleOwnerLogin . repoOwner $ fromJust repo)
       (repoName (fromJust repo))
       (pullRequestNumber dpr)
  where
    repo = pullRequestCommitRepo . pullRequestBase $ dpr


-- | Converts a GitHub pull request to a mail thread and sends it.
pullRequestToThread :: Maybe Auth -- ^ Github authentication
                    -> PRID             -- ^ wich PR to convert to an email
                                        --   thread
                    -> Text           -- ^ recipient email address
                    -> Maybe Text     -- ^ reply-to address
                    -> Maybe Text     -- ^ post-checkout hook program
                    -> IO ThreadInfo
pullRequestToThread m'auth prid recipient replyTo checkoutHookCmd = do
  pr <- downloadPullRequest m'auth prid
  prevThreadInfo <- getMostRecentThreadInfo m'auth prid
  sendPatchSeries recipient replyTo prevThreadInfo checkoutHookCmd pr
