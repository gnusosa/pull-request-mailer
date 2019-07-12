{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

module Github.PullRequests.Mailer.Opts
  ( Opts(..)
  , tokenEnvVar
  , secretEnvVar
  , optsParser
  , pridParser
  , parseOptsAndEnv
  ) where

import GitHub.Auth
import GitHub.Data.Definitions
import qualified Data.ByteString.Char8 as BS8
import Options.Applicative
import qualified Data.ByteString as BS
import qualified Options.Applicative.Help as H
import System.Environment (lookupEnv)

import Github.PullRequests.Mailer


-- | Command line arguments to this program.
data Opts = Opts
  { optsRecipient          :: String
  , optsReplyTo            :: Maybe String
  , optsPostCheckoutHook   :: Maybe String
  , optsAuth               :: Maybe Auth
  , optsNoThreadTracking   :: Bool
  , optsDiscussionLocation :: Maybe String
  , optsSecret             :: Maybe String
  } deriving (Eq, Ord, Show)


-- | Env variable that can set the auth token.
tokenEnvVar :: String
tokenEnvVar = "PULL_REQUEST_MAILER_OAUTH_TOKEN"


-- | Env variable that can set the secret webhook validation token.
secretEnvVar :: String
secretEnvVar = "PULL_REQUEST_MAILER_SECRET_TOKEN"


-- | Command line argument parser.
optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
        ( long "to"
          <> metavar "EMAIL"
          <> help "Email recipient"
        )
  <*> optional (strOption
        ( long "reply-to"
          <> metavar "EMAIL"
          <> help "Address to which responses shall be sent. Useful if\
                  \ the sending email address shall not receive replies."
        )
      )
  <*> optional (strOption
        ( long "post-checkout-hook"
          <> metavar "PROGRAM"
          <> help "A program in the cloned direcotry just after checkout"
        )
      )
  <*> pure Nothing -- set by env variable
  <*> switch
        ( long "no-thread-tracking"
          <> help "Disable posting thread message ID and patch iteration\
                  \ count into the pull request. When active, future versions\
                  \ of the PR can not be sent as reply to the created email\
                  \ thread"
        )
  <*> optional (strOption
        ( long "discussion-location"
          <> metavar "STRING"
          <> help "The place where the contents of the PR are discussed (as\
                  \ opposed to the discussion being in PR comments. Example:\
                  \ 'the mailing list project@example.com'."
        )
      )
  <*> pure Nothing -- set by env variable


-- | Command line argument parser for pull request identifiers.
pridParser :: Parser PRID
pridParser =
  PRID
    <$> argument str
          ( metavar "USER"
            <> help "GitHub user who owns the repo containing the pull request"
          )
    <*> argument str
          ( metavar "REPO"
            <> help "Repo containing the pull request"
          )
    <*> pridParserIssueNumber

pridParserIssueNumber :: Parser IssueNumber
pridParserIssueNumber = IssueNumber <$>
                  argument auto
                  ( metavar "N"
                    <> help "Number of the pull request"
                  )

-- | Like `execParser`, but sets those fields of `Opts` that can be set via
-- environment variables.
--
-- It allows extending the `Parser` as well as updating the help text
-- (`InfoMod`); a help text with the full program description is the default.
--
-- Common usage:
--
-- >opts <- parseOptsAndEnv id (progDesc "This program does...")
parseOptsAndEnv :: (Parser Opts -> Parser a) -> InfoMod a -> IO a
parseOptsAndEnv f infoMod = do
  envToken <- lookupEnv tokenEnvVar
  envSecret <- lookupEnv secretEnvVar
  let setEnvOpts opts = opts{ optsAuth = OAuth <$> BS8.pack <$> envToken
                            , optsSecret = envSecret
                            }
  execParser $
    info
    (helper <*> f (setEnvOpts <$> optsParser))
    ( fullDesc
        <> footerDoc
             (H.unChunk $ H.vcatChunks
                [ H.stringChunk "Available environment variables:"
                , H.tabulate
                    [ ( H.text tokenEnvVar
                      , H.align . H.extractChunk $ H.paragraph
                          "Auth token needed to write information\
                          \ into the pull request.\
                          \ You can generate one at\
                          \ https://github.com/settings/applications."
                      )
                    , ( H.text secretEnvVar
                      , H.align . H.extractChunk $ H.paragraph
                          "Secret token to verify that requests really come\
                          \ from Github. See\
                          \ https://developer.github.com/webhooks/securing."
                      )
                    ]
                ]
             )
        <> infoMod
    )
