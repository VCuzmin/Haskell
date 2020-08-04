data StringTree = StringTree String [StringTree] -- a tree of strings

hierarchy :: StringTree
hierarchy = StringTree "C:"
            [ StringTree "Program Files" []
            ,StringTree "Users" [StringTree "Alice" []]
            ,StringTree "Cats" []
            ]