1.4.6
  - Add a decoder combinator named `optional` for optional fields of primitive type.

1.4.5
  - Add encoders for packed repeated fields that can iterate in
    reverse order for speed when practical and will predict size
    when it is inexpensive to do so.
  - Add `bytesIfNonempty`, `etaMessageBuilder`, `unsafeFromByteString`,
    `unsafeFromShortByteString`, `repeatedMessageBuilder`.

1.4.4
  - Support GHC 9.10

1.4.3
  - Support GHC 9.8
  - Support GHC 9.6

1.4.2
  - Support GHC 9.4

1.4.1
  - Support ShortByteString and ShortText

1.4.0
  - Improve decoding performance
  - Remove internal toMap function

1.3.0
  - Support GHC 9.2
  - Prevent inlining for GHCJS

1.2.2
  - Add new `zigZag{Encode,Decode}` utilities

1.2.1
  - Build against GHC 9.0
  - Build against `tasty` 1.3 and 1.4

1.2.0
  - Encode protobuf wire messages in reverse order to improve perfomance
  - Miscellaneous maintenance changes
