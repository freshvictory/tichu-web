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
        , background = "FFF"
        , menuBackground = "EEE"
        , cta = "DBB004"
        , ctaText = "000"
        , text = "000"
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
        , menuBackground = "8febbc"
        , text = "734941"
        , cta = "734941"
        , ctaText = "bffec6"
        , pop = "734941"
        }
    }
