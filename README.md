# HSolitaire

A Haskell (console WIP) version of Klondike Solitaire

- TODO 1 stack app
- TODO 2 String -> Text
        ++ becomes <> from Data.Monoid
        Independent of that I heartily recommend using generic functions, aka functions on typeclasses rather that functions on type. Examples are using mappend (or the operator version <>) rather than ++ or Text.append, mempty rather than [] or Text.empty and fmap rather than map.
        
https://www.reddit.com/r/haskell/comments/3rrlih/text_or_string/
https://github.com/snoyberg/basic-prelude#readme

- TODO 2 change Maybe Deck with Either Deck Error (Error = String/Text)

- TODO 2 no color version
- TODO 3 Testing
- TODO 2 README
- TODO 5 Different deck designs (Back & Front)
- TODO 5 Configurable commands
- TODO 5 Multilanguage
- TODO 5 Saveable options
