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
    { border = hex "#F288C2"
    , background = hex "F2B6DD"
    , menuBackground = hex "F24B99"
    , text = hex "ffeb09"
    , cta = hex "#5CE3F2"
    , ctaText = hex "000"
    , pop = hex "ffeb09"
    }
  }
