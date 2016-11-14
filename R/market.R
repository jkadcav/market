library(jsonlite)
#library(hash)

e <- new.env()
meeting_events<-''
meeting_id<-''
race_number<-''

#' Retrieve DW market data
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @keywords markets
#' @export
#' @examples
#' retrieve_event(1,109501,10962625,'MTX PIR ACTUAL')
retrieve_markets<-function( race, meetingId ){
  meeting_events <- get("meeting_events", envir = e)
  meeting_id <- get("meeting_id", envir = e)
  race_number <- get("race_number", envir = e)

  if( meetingId==meeting_id && race == race_number ) {
    events<-meeting_events
  } else {
    events <- jsonlite::fromJSON(paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/markets?event_number=",race,"&meeting_id=",meetingId,sep=""))
    assign("meeting_events", events, envir = e)
    assign("meeting_id", meetingId, envir = e)
    assign("race_number", race, envir = e)
  }

  return(events)
}

#' Retrieve DW market data for a specific competitor
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @param compEventId event competitor ID from DW API
#' @param providerShortCode provider short_code e.g. dw
#' @param attributeName attribute name to retrieve e.g. values or positions
#' @param dwItemName value to retrieve from DW market attribute
#' @keywords markets
#' @export
#' @examples
#' retrieve_market_values(1,109501,10962625,'dw', 'positions','MTX PIR ACTUAL')
retrieve_market_values<-function( race, meetingId, providerShortCode, attributeName, dwItemName ){
  events<-retrieve_markets( race, meetingId )

  a<-paste('events$data$',providerShortCode,'$`',dwItemName,'`$market$',attributeName,sep="")
  positions<-eval(parse(text=a))
  if (length(positions)<1) return(NA)
  else return(positions)
}

#' Retrieve DW market data for a specific competitor
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @param compEventId event competitor ID from DW API
#' @param providerShortCode provider short_code e.g. dw
#' @param attributeName attribute name to retrieve e.g. values or positions
#' @param dwItemName value to retrieve from DW market attribute
#' @keywords markets
#' @export
#' @examples
#' retrieve_competitor_market_values(1,109501,10962625,CID, 'dw', 'positions', 'MTX PIR ACTUAL')
retrieve_competitor_market_values<-function( race, meetingId, compEventId, providerShortCode, attributeName, dwItemName ){
  events<-retrieve_markets( race, meetingId )
  a<-paste('events$data$',providerShortCode,'$`',dwItemName,'`$market$',attributeName,'$`',compEventId,'`',sep="")
  mtx<-eval(parse(text=a))[[2]]
  if(length(mtx)<1) return(NA)
  else return(mtx)
}

#' Retrieve DW market data for all competitors in an event
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @param dwItemName value to retrieve from DW market attribute
#' @keywords markets
#' @export
#' @examples
#' retrieve_matrix_values(1,109501,10962625)
retrieve_matrix_values<-function(race,meetingId){
  val<-retrieve_market_values(race,meetingId,'dw', 'positions','MTX PIR ACTUAL')
  if (length(val)<1) return(NA)
  else return(val)
}

#' Retrieve DW market data
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @param compEventId event competitor ID from DW API
#' @param dwItemName value to retrieve from DW market attribute
#' @keywords markets
#' @export
#' @examples
#' retrieve_competitor_matrix_value(1,109501,10962625)
retrieve_competitor_matrix_value<-function(race,meetingId,compEventId){
  val<-retrieve_competitor_market_values(race,meetingId,compEventId,'dw', 'positions','MTX PIR ACTUAL')
  if(length(val)<1) return(NA)
  else return(val)
}

#' Retrieve TAB_VIC market prices
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @keywords markets
#' @export
#' @examples
#' retrieve_prices(1,109501)
retrieve_prices<-function(race,meetingId ){
  retrieve_market_values( race, meetingId, 'tab_vic', 'prices','WIN')
  events<-retrieve_markets( race, meetingId )
  a<-paste('events$data$tab_vic$WIN$market$prices',sep="")
  prices<-eval(parse(text=a))
  prices<-prices[prices!=0]
  return(prices)
}

#' Retrieve exlcude race indicator
#' @param race event number 1....13
#' @param meetingId meeting ID from DW API
#' @keywords exclude race
#' @export
#' @examples
#' retrieve_exclude(1,109501)
retrieve_exclude<-function(race,meetingId){
  events<-retrieve_markets( race, meetingId )
  a<-paste('events$data$dw$`MTX SOLUTION PARAMS`$market$values$excludeRace')
  exclude<-eval(parse(text=a))
  if(length(exclude)<1) return(1)
  else if(exclude=='FALSE') return(1)
  else if(exclude=='TRUE') return(0)
}
