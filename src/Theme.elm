module Theme exposing
  ( ThemeSettings
  , light
  , dark
  , strawberry
  , mint
  )

import Css exposing (Color, hex)

type alias ThemeSettings =
  { id: String
  , name: String
  , colors: Colors
  }


type alias Colors =
  { border: Color
  , background: Color
  , menuBackground: Color
  , cta: Color
  , ctaText: Color
  , text: Color
  , pop: Color
  }


light : ThemeSettings
light =
  { id = "light"
  , name = "Light"
  , colors =
    { border = hex "BBB"
    , background = hex "FFF"
    , menuBackground = hex "EEE"
    , cta = hex "DBB004"
    , ctaText = hex "000"
    , text = hex "000"
    , pop = hex "B84444"
    }
  }


dark : ThemeSettings
dark =
  { id = "dark"
  , name = "Dark"
  , colors =
    { border = hex "444"
    , background = hex "000"
    , menuBackground = hex "323232"
    , text = hex "CCC"
    , cta = hex "DBB004"
    , ctaText = hex "000"
    , pop = hex "B84444"
    }
  }


strawberry : ThemeSettings
strawberry =
  { id = "strawberry"
  , name = "Strawberry"
  , colors =
    { border = hex "ef5b95"
    , background = hex "f7a3cc"
    , menuBackground = hex "f67fb6"
    , text = hex "FFF"
    , cta = hex "d53032"
    , ctaText = hex "FFF"
    , pop = hex "9cbe45"
    }
  }


mint : ThemeSettings
mint =
  { id = "mint"
  , name = "Mint"
  , colors =
    { border = hex "734941"
    , background = hex "deffe1"
    , menuBackground = hex "8febbc"
    , text = hex "734941"
    , cta = hex "734941"
    , ctaText = hex "bffec6"
    , pop = hex "734941"
    }
  }
