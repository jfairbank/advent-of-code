module NonEmptyList exposing (NonEmptyList, new)


type NonEmptyList a
    = NonEmptyList a (List a)


new : a -> NonEmptyList a
new a =
    NonEmptyList a []
