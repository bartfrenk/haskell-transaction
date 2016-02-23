module Transaction.Plot
       ( transformAxisData )
       where

import Graphics.Rendering.Chart.Axis.Types

transformAxisData :: (AxisData a) -> (a -> b) -> (b -> a) -> AxisData b
transformAxisData axisData injection section = AxisData {
  _axis_visibility = (_axis_visibility axisData),
  _axis_viewport = \r y -> (_axis_viewport axisData) r $ section y,
  _axis_tropweiv = \r d -> injection $ (_axis_tropweiv axisData) r d,
  _axis_ticks = mapFirst injection (_axis_ticks axisData),
  _axis_labels = map (mapFirst injection) (_axis_labels axisData),
  _axis_grid = map injection (_axis_grid axisData)
  }
  where mapFirst f pairs = map (\(u, v) -> (f u, v)) pairs
