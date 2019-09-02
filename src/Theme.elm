module Theme exposing
  ( ThemeSettings
  , light
  , dark
  , strawberry
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
    , text = hex "ffeb09"
    , cta = hex "71be44"
    , ctaText = hex "000"
    , pop = hex "ffeb09"
    }
  }
