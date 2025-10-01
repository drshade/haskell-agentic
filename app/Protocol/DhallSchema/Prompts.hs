module Protocol.DhallSchema.Prompts where

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
      """ prompt dhallSchema

languageReference1 :: Text
languageReference1 =
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
      Date, Time and TimeZone are built-in types and can be represented such as:
      When asked to perform calculations - always attempt to write a function to compute the answer
      When asked to return logical responses - always attempt to write a function to compute the answer

      --- BEGIN WORK IN PROGRESS ---
      Available datatypes and functions:
      Bool
         - keyword: if True then 3 else 5
         - operators: ||, &&, ==, != (these only work on Bool, no other type, do not attempt to use them with other types)
      Natural
         - operators: +, *
         - functions: Natural/even, Natural/odd, Natural/isZero, Natural/subtract, Natural/fold, Natural/build, Natural/show, Natural/toInteger
      Integer
         - functions: Integer/negate, Integer/clamp, Integer/toDouble, Integer/show
      Double
         - functions: Double/show
      Text
         - functions: Text/show, Text/replace
         - operators: ++
      Date
         - functions: Date/show
      Time
         - functions: Time/show
      TimeZone
         - functions: TimeZone/show
      List
         - functions: List/fold, List/build, List/length, List/head, List/last, List/indexed, List/reverse
         - operators: #
      Record
         - operators: ., //\\\\, /\\, //, ::
         - keywords: toMap, with

      These are strictly typed, and so for example you cannot use operator + to add two Integer types, as + only works with Natural.
      You cannot compare two Text's with == either (only Bools), you should rather introduce an enumeration for this.
      To add two Integers together you must import the add function from the prelude
      for example:
         let add = https://prelude.dhall-lang.org/Integer/add
          in add +1 +4

      List/fold example:
      List/fold Bool [True, False, True] Bool (\\(x : Bool) -> λ(y : Bool) -> x && y) True

      You cannot compare Text using operator (==).

      If you use any of the functions related to prelude, then you must include:
      let Prelude = https://prelude.dhall-lang.org/package.dhall

      And then you can use these additional functions such as:
      Integer.abs: Integer → Natural
      Integer.add: Integer → Integer → Integer
      Integer.clamp: Integer → Natural
      Integer.equal: Integer → Integer → Bool
      Integer.greaterThan: Integer → Integer → Bool
      Integer.greaterThanEqual: Integer → Integer → Bool
      Integer.lessThan: Integer → Integer → Bool
      Integer.lessThanEqual: Integer → Integer → Bool
      Integer.multiply: Integer → Integer → Integer
      Integer.negate: Integer → Integer
      Integer.negative: Integer → Bool
      Integer.nonNegative: Integer → Bool
      Integer.nonPositive: Integer → Bool
      Integer.positive: Integer → Bool
      Integer.show: Integer → Text
      Integer.subtract: Integer → Integer → Integer
      Integer.toDouble: Integer → Double
      Integer.toNatural: Integer → Optional Natural
      Text.concatSep: ∀(separator : Text) → ∀(elements : List Text) → Text

      --- END WORK IN PROGRESS ---

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

languageReference1_no_functions :: Text
languageReference1_no_functions =
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


languageReference2 :: Text
languageReference2 = 
   """
   Dhall Language Reference:
   - Strongly typed functional configuration language with type inference
   - Syntax: λ(x : T) -> body for functions, : for type annotations, . for record access
   - Primitives: Bool (True/False), Natural (0,1,2...), Integer (+1,-5), Double, Text, Date, Time, TimeZone
   - Collections: List T, Optional T, Records {field:Type}, Unions <Tag:Type|Other>
   - Operators: Bool (&&,||,==,!=), Natural (+,*), Text (++), List (#), Record (∧,⫽,//)
   - Keywords: let, in, if/then/else, merge, Some/None, with, toMap
   - Functions: Natural/fold, List/length, Text/replace, Integer/show, etc.
   - String literals: "text" or multiline '' text '' (escape '' with ''')
   - Type constraints: Natural + only works with Natural, not Integer
   - Imports: https://prelude.dhall-lang.org/... for extended functions
   - Schema adherence: Always include type annotations and let bindings as shown in examples

   Examples:
   -- Record with optional field and union
   let Status = < Active | Inactive >
   let User = { name : Text, age : Natural, status : Status, bio : Optional Text }
   in { name = "Alice", age = 30, status = Status.Active, bio = Some "Developer" } : User

   -- Function with conditional logic  
   let canVote = λ(age : Natural) -> Natural/isZero (Natural/subtract age 18)
   in canVote 21

   -- List processing with fold
   let numbers = [1, 2, 3, 4] : List Natural
   let sum = List/fold Natural numbers Natural (λ(x : Natural) -> λ(acc : Natural) -> x + acc) 0
   in { total = sum, count = List/length Natural numbers }

   When using the same union type repeatedly, introduce a let binding for it:
   Example: Instead of repeating < Red | Green | Blue >.Red everywhere,
   use: let Color = < Red | Green | Blue > in Color.Red
   
   IMPORTANT: Return ONLY valid Dhall code - no markdown code blocks, no explanations, no preamble.
   """
