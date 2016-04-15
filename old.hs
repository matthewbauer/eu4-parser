
toMap x = fromList $ mapMaybe toTuple x
  where
    toTuple (ClausewitzText.Assignment a b) = Just (a, b)
    toTuple _ = Nothing

width m = case m ! "width" of ClausewitzText.Float f -> f
height m = case m ! "height" of ClausewitzText.Float f -> f
maxProvinces m = case m ! "max_provinces" of ClausewitzText.Float f -> f

lookupStr k m = case m ! k of ClausewitzText.String f -> f

ssvOptions = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral $ ord ';' }
adjacencies m = (CSV.decodeWith ssvOptions CSV.HasHeader :: BSL.ByteString ->
            Either String (Vector (Int, Int, BSL.ByteString,
            Int, Int, Int, Int, Int, BSL.ByteString)))
            <$> BSL.readFile (prefix $ lookupStr "adjacencies" m)
definitions m = (CSV.decodeWith ssvOptions CSV.HasHeader :: BSL.ByteString ->
            Either String (Vector (Int, Word8, Word8, Word8, BSL.ByteString)))
            <$> BSL.readFile (prefix $ lookupStr "definitions" m)

colorN defs n = color <$> find findN defs
  where findN (a, b, c, d, e) = a == n
        color (a, b, c, d, e) = (b, c, d)

area m = (toMap <$>) <$> readAndParseC (lookupStr "area" m)
climate m = (toMap <$>) <$> readAndParseC (lookupStr "climate" m)
continent m = (toMap <$>) <$> readAndParseC (lookupStr "continent" m)
positions m = (toMap <$>) <$> readAndParseC (lookupStr "positions" m)
region m = (toMap <$>) <$> readAndParseC (lookupStr "region" m)
seasons m = (toMap <$>) <$> readAndParseC (lookupStr "seasons" m)
superregion m = (toMap <$>) <$> readAndParseC (lookupStr "superregion" m)
tradeWinds m = (toMap <$>) <$> readAndParseC (lookupStr "trade_winds" m)

provinces m = do
  Right bmp <- readBMP $ prefix $ lookupStr "provinces" m
  return $ unpackBMPToRGBA32 bmp

pixel p w (x, y) = (c i, c (i+1), c (i+2))
  where i = floor (x `mod'` w + w * y) * 4
        c = BS.index p

-- map doesn't
-- https://en.wikipedia.org/wiki/Miller_cylindrical_projection
yToLat y = 180 / pi * 2.5 * atan (exp (pi / 180 * y * 4 / 5)) - 5 * pi / 8
yToLat y  = (y - 1024) / 1024 * 80
xToLong x = (x - 2810) / 2810 * 180
