module Theme exposing
    ( ThemeSettings
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
    , lightShadow : String
    , darkShadow : String
    , menuBackground : String
    , cta : String
    , ctaText : String
    , text : String
    , pop : String
    }


light : ThemeSettings
light =
    { id = "light"
    , name = "Light"
    , colors =
        { border = "BBB"
        , background = "EFEFEF"
        , lightShadow ="FFFFFFAA"
        , darkShadow = "60606026"
        , menuBackground = "DDD"
        , cta = "DBB004"
        , ctaText = "000"
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
        , background = "000"
        , lightShadow ="494949"
        , darkShadow = "60606026"
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
        , lightShadow ="f8add2"
        , darkShadow = "60606033"
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
        , lightShadow ="deffe1"
        , darkShadow = "7f7f7f26"
        , menuBackground = "8febbc"
        , text = "734941"
        , cta = "734941"
        , ctaText = "bffec6"
        , pop = "734941"
        }
    }
