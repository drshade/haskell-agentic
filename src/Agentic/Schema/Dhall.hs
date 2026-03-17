{-# LANGUAGE MultilineStrings #-}
module Agentic.Schema.Dhall
  ( dhallSchemaOf
  , parseDhall
  , dhallSystemPrompt
  , injectDhallSchema
  , injectDhallObject
  , dhallRetryPrompt
  ) where

import Agentic.Error (SchemaError(..))
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Either.Validation (Validation(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core

-- | Render the Dhall type schema for @a@ as Text.
dhallSchemaOf :: forall a. (FromDhall a, ToDhall a) => Proxy a -> Text
dhallSchemaOf _ = case Dhall.expected (Dhall.auto @a) of
  Success result -> Dhall.Core.pretty result
  Failure err    -> error $ show err

-- | Parse a Dhall text value into @a@, returning a SchemaError on failure.
parseDhall :: forall a. FromDhall a => Text -> IO (Either SchemaError a)
parseDhall input = do
  result <- try @SomeException $ Dhall.input Dhall.auto input
  case result of
    Right value -> pure $ Right value
    Left err    -> case fromException @SomeAsyncException err of
      Just async -> throwIO async
      Nothing    -> pure $ Left $ DhallParseError $ pack $ show err

-- | Inject the Dhall schema for @a@ into a user prompt.
injectDhallSchema :: forall a. (FromDhall a, ToDhall a) => Proxy a -> Text -> Text
injectDhallSchema proxy prompt =
  prompt <> "\n\nReturn response in Dhall format using the following schema:\n" <> dhallSchemaOf proxy

-- | Inject a Dhall-serialised value into a prompt.
injectDhallObject :: forall a. ToDhall a => a -> Text -> Text
injectDhallObject obj prompt =
  let dhallValue = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
  in prompt <> "\n\nInput:\n" <> dhallValue

-- | Build a retry prompt from the parse error, bad response, and original instruction.
dhallRetryPrompt :: Text -> Text -> Text -> Text
dhallRetryPrompt err reply originalInstruction =
  "Your last reply failed Dhall parsing with the following error:\n" <> err
  <> "\n\nYour response was:\n" <> reply
  <> "\n\nThe instruction you were given was:\n" <> originalInstruction
  <> "\n\nPlease fix the problem and respond with the correct output"

-- | System prompt instructing the LLM to reply in Dhall format.
dhallSystemPrompt :: Text
dhallSystemPrompt =
   pack
      """
      You reply to all responses in Dhall format only, do not include any markdown or any other lead-in syntax. Just output pure Dhall as a bare string.

      --------
      Dhall language reference
      --------
      Examples:
      Example schema: < Dog : { name : Text, age : Natural, breed : Text } | Cat : { name : Text } >
      Valid response:
         let Schema = < Dog : { name : Text, age : Natural, breed : Text }
                        Cat : { name : Text } >
         in Schema.Dog { name = "Rex", age = 7, breed = "schauzer" }
      Invalid response:
         Dog { name = "Rex", age = 7, breed = "schauzer" }

      Example schema: { name : Text, age : Integer, maritalStatus : < Unmarried | Married | Widowed > }
      Valid response:
         let Schema = { name : Text, age : Integer, maritalStatus : < Unmarried | Married | Widowed > }
         in { name = "Jane Doe", age = +29, maritalStatus = < Unmarried | Married | Widowed >.Unmarried } : Schema
      Valid response:
         let MaritalStatus = < Unmarried | Married | Widowed >
         let Schema = { name : Text, age : Integer, maritalStatus : MaritalStatus }
         in { name = "Jane Doe", age = +29, maritalStatus = MaritalStatus.Unmarried } : Schema

      Example schema: List { name : Text, description : Optional Text }
      Valid response:
         let Schema = { name : Text, description : Optional Text }
         in [ { name = "Pizza", description = Some "Tasty!" }, { name = "Bangers 'n mash", description = None Text } ] : List Schema
      Valid response:
         let Schema = { name : Text, description : Optional Text }
         let mkFood = \\(n : Text) ->
               let description = None Text
               in { name = n, description }
         in [ mkFood "pizza", mkFood "burgers" ] : List Schema

      Example schema: { _1 : { name : Text }, _2 : < Unmarried | Married > }
      Valid response:
         let MaritalStatus = < Unmarried | Married >
         let Schema = { _1 : { name : Text }, _2 : MaritalStatus }
         in { _1 = { name = "Robert"}, _2 = MaritalStatus.Unmarried } : Schema
      Invalid response:
         let Schema = { _1 : { name : Text }, _2 : MaritalStatus }
         in { _1 = { name = "Robert"}, _2 = Schema._2.Unmarried } : Schema

      Example schema: { d : Date, t : Time, tz : TimeZone}
      Valid response:
         let Schema = { d : Date, t : Time, tz : TimeZone }
         in { d = 2025-12-25
            , t = 00:00:00.0
            , tz = +02:00
            } : Schema

      Example schema: { name: Text, pic : Text }
      Valid response:
         let Schema = { name: Text, pic : Text }
         in { name = "Dino"
            , pic =
            ''
            __
            / ")
      .-^^^-/ /
   __/       /
   <__.'_'''_'
            ''
            } : Schema
      (Notice the triple single quote above, which escapes the double single quote to avoid breaking the multiline string)

      Example schema: { name : Text, description : Text }
      Valid response:
      let Schema = { name : Text, description : Text }
      in { name = "Velociraptor"
         , description =
         ''
         Velociraptor is a small, bipedal dromaeosaurid theropod from the Late Cretaceous (about 75 to 71 million years ago), known mainly from Mongolia's Djadokhta and Barun Goyot formations. Adults were roughly 2.0 m long, about 0.5 m at the hip, and are estimated at around 15 kg. Distinctive features include a long, stiffened tail, an enlarged recurved "sickle" claw on the second toe, and evidence of feathers (quill knobs on the ulna). A famous fossil shows a Velociraptor locked in combat with a Protoceratops. First described in 1924 (Velociraptor mongoliensis), it is often misrepresented in popular media as much larger than it actually was.
         ''
         } : Schema

      Extra syntax rules:
      Escape double-quotes with backslash (but not single quotes) in Text
      Multiline strings start and end with '' on a blank line.
      Single and double quotes can be used without escaping in a multiline string, except for two single quotes which must be escaped by inserting three single quotes.
      Natural numbers need no prefixes but Integers always need the sign prefixed (e.g. +10 or -100)

      Additionally - try to be as uncreative as possible when abiding by the schema.
      e.g. if there is only field and the prompt has asked for many, don't try to
      squash all the results into this single field. Just insert one.
      Do not break the contract of the schema as it will not be interpretted by a
      human or other agent, but rather a structured parser.
      You may introduce your own variables and functions using let syntax (see examples) for
      dealing with repeated values and/or to save space.
      If asked to do repetitive work, generally you should introduce a function to
      construct the output.

      When asked to invoke tools, or when responding to queries where tool output is provided
      do not forget to respond using the Dhall schema provided. This is a non-negotiable condition
      to all responses. This is the only protocol you can respond using.
      """
