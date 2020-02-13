module Theme exposing
    ( NeumorphicShadow
    , ThemeSettings
    , dark
    , light
    , mint
    , strawberry
    )


type alias ThemeSettings =
    { id : String
    , name : String
    , colors : Colors
    }


type alias Colors =
    { border : String
    , background : String
    , lightShadow : NeumorphicShadow
    , darkShadow : NeumorphicShadow
    , ctaShadow : NeumorphicShadow
    , menuBackground : String
    , cta : String
    , ctaText : String
    , text : String
    , pop : String
    }


type alias NeumorphicShadow =
    { light : String
    , dark : String
    }


light : ThemeSettings
light =
    { id = "light"
    , name = "Light"
    , colors =
        { border = "BBB"
        , background = "EFEFEF"
        , lightShadow = { light = "ffffff4a", dark = "60606026" }
        , darkShadow = { light = "ffffff4a", dark = "60606026" }
        , ctaShadow = { light = "feedaa", dark = "dbb004a8" }
        , menuBackground = "DDD"
        , cta = "DBB004"
        , ctaText = "4d4d4d"
        , text = "4d4d4d"
        , pop = "B84444"
        }
    }


dark : ThemeSettings
dark =
    { id = "dark"
    , name = "Dark"
    , colors =
        { border = "444"
        , background = "111111"
        , lightShadow = { light = "161616", dark = "050505" }
        , darkShadow = { light = "ffffff4a", dark = "60606026" }
        , ctaShadow = { light = "fde78d59", dark = "DBB00459" }
        , menuBackground = "323232"
        , text = "CCC"
        , cta = "DBB004"
        , ctaText = "000"
        , pop = "B84444"
        }
    }


strawberry : ThemeSettings
strawberry =
    { id = "strawberry"
    , name = "Strawberry"
    , colors =
        { border = "ef5b95"
        , background = "f7a3cc"
        , lightShadow = { light = "f8add2", dark = "60606033" }
        , darkShadow = { light = "f8add25e", dark = "60606033" }
        , ctaShadow = { light = "e57f80", dark = "a82224aa" }
        , menuBackground = "f67fb6"
        , text = "FFF"
        , cta = "d53032"
        , ctaText = "FFF"
        , pop = "9cbe45"
        }
    }


mint : ThemeSettings
mint =
    { id = "mint"
    , name = "Mint"
    , colors =
        { border = "734941"
        , background = "deffe1"
        , lightShadow = { light = "ffffff7d", dark = "9797977a" }
        , darkShadow = { light = "a3a3a36e", dark = "4242426e" }
        , ctaShadow = { light = "bc908899", dark = "5f3c368f" }
        , menuBackground = "8febbc"
        , text = "734941"
        , cta = "734941"
        , ctaText = "bffec6"
        , pop = "734941"
        }
    }
