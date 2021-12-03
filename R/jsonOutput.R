JSON <- function(
  loadCurves
  ,loadCurveSets
  ,channelInformation
  ,values
) {
  sprintf(
    '{"loadCurves":[%s"loadCurveSets":[%s"channelInformation":%s,"values":[%s]}]}]}'
    ,loadCurves, loadCurveSets, channelInformation, values)
}