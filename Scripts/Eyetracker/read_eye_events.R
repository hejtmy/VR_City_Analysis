read_eye_events = function(text){
  MSG_indexes = grep('^MSG\\t+.*', text)
  CAL_indexes = grep('\\!CAL+.*', text)
  #removing calibration indexes from the MSG
  MSG_indexes = MSG_indexes[!(MSG_indexes %in% CAL_indexes)]
  text = text[MSG_indexes]
  
  msgs <- list()
  #adds everything into a list
  for (line in text){
    msg <- strsplit(line, "[\t ]")
    msgs <- c(msgs,msg)
  }
  
  output_list = list()
  
  mouse_move_idx = sapply(msgs, hasWord, 'MOUSE_MOVE')
  mouse_updown_idx = (sapply(msgs, hasWord,'MOUSE_UP') | sapply(msgs,hasWord,'MOUSE_DOWN'))
  key_updown_idx = (sapply(msgs, hasWord, 'KEY_UP') | sapply(msgs, hasWord, 'KEY_DOWN'))
  if(sum(mouse_move_idx) != 0){
    mouse_move_events = data.table(matrix(unlist(msgs[mouse_move_idx]), nrow = sum(mouse_move_idx), byrow=T))
  } else { mouse_move_events = NULL}
  if(sum(mouse_updown_idx) != 0){
    mouse_updown_events = data.table(matrix(unlist(msgs[mouse_updown_idx]), nrow=sum(mouse_updown_idx), byrow=T))
  } else { mouse_updown_events = NULL}
  if(sum(key_updown_idx) != 0){
    key_updown_events = data.table(matrix(unlist(msgs[key_updown_idx]), nrow=sum(key_updown_idx), byrow=T))
  } else { key_updown_events = NULL}
  rest_events_idx = !(mouse_move_idx | mouse_updown_idx | key_updown_idx)
  
  #dropping the first useless column
  
  output_list = list('mouse_move_events' = mouse_move_events, 'mouse_updown_events' = mouse_updown_events, 'key_updown_events' = key_updown_events, 'rest_events' = msgs[rest_events_idx])
  
  return(output_list)
}