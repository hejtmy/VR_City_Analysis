prep_eye_data = function(ls){
  ls$events = prep_eye_events(ls$events)
  return(ls)
}