module Prompts where

import Data.Text
import Text.Printf (printf)

injectObject :: Text -> Text -> Text
injectObject prompt object =
    pack $ printf
        """
        %s

        Input:
        %s
        """ prompt object
    --prompt' <> "\n\n" <> instructions <> "\n" <> dhallSchema @s <> "\n\n" <> examples <> "\n\n" <> syntax <> "\n\n" <> extras

retryError :: Text -> Text -> Text -> Text
retryError err reply input =
    pack $ printf
        """
        Your last reply failed Dhall parsing with the following error:
        %s

        Your response was:
        %s
        
        The instruction you were given was:
        %s

        Please fix the problem and respond with the correct output
        """ err reply input

injectDhallSchema :: Text -> Text -> Text
injectDhallSchema prompt dhallSchema =
    pack $ printf
        """
        %s

        Return response in Dhall format using the following schema:
        %s

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
        """ prompt dhallSchema
