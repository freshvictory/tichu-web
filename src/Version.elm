module Version exposing (Version, versionDecoder, compareVersion)

import Json.Decode exposing
  (Decoder, Value, decodeValue, succeed, map6, field, string, int, list)


type alias Version =
  { version: String
  }


versionDecoder : Decoder Version
versionDecoder =
  Json.Decode.map Version (field "version" string)


compareVersion : Version -> Version -> Bool
compareVersion ours theirs =
  let
    ourStrings = String.split "." ours.version
    ourNumbers = List.map
      (\s ->
        case String.toInt s of
          Just num -> num
          Nothing -> 0
      )
      ourStrings
    theirStrings = String.split "." theirs.version
    theirNumbers = List.map
      (\s ->
        case String.toInt s of
          Just num -> num
          Nothing -> 0
      )
      theirStrings
  in
    if List.length theirNumbers /= List.length ourNumbers then
      False
    else if (
      List.foldr
        (&&)
        True
        (List.indexedMap
          (\i n ->
            case (List.head (List.drop i ourNumbers)) of
              Just num -> if i == (List.length theirNumbers) - 1 then num < n else num <= n
              Nothing -> False
          )
          theirNumbers
        )
    ) then
      True
    else
      False

  