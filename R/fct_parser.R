library(jsonlite)

flatten_schema <- function(schema) {
  flattened_schema <- schema

  flatten_property <- function(prop) {
    if (!is.null(prop$allOf)) {
      # Merge all properties from allOf
      merged_prop <- list()
      for (item in prop$allOf) {
        if (!is.null(item$`$ref`)) {
          # Resolve reference and merge properties
          ref_path <- strsplit(sub("^#/", "", item$`$ref`), "/")[[1]]
          ref_def <- Reduce(`[[`, ref_path, schema)
          merged_prop <- modifyList(merged_prop, ref_def)
        } else {
          # Merge non-reference properties
          merged_prop <- modifyList(merged_prop, item)
        }
      }
      return(merged_prop)
    }
    return(prop)
  }

  # Flatten oneOf properties
  if (!is.null(flattened_schema$oneOf)) {
    flattened_schema$oneOf <- lapply(flattened_schema$oneOf, function(event) {
      event$properties <- lapply(event$properties, flatten_property)
      return(event)
    })
  }

  return(flattened_schema)
}

#' Parse JSON schema
#'
#' @param schema A list representing the JSON schema
#' @return A parsed schema structure
parse_json_schema <- function(schema) {
  parsed_schema <- list()

  # Iterate through each event in the schema's oneOf array
  for (event in schema$oneOf) {
    event_type <- event$properties$mgmt_operations_event$const
    parsed_schema[[event_type]] <- parse_event(event, schema)
  }

  return(parsed_schema)
}


#' Parse individual event
#'
#' @param event A list representing an event in the schema
#' @return A parsed event structure
parse_event <- function(event, schema) {
  parsed_event <- list(
    title = get_multilingual_field(event, "title"),
    properties = list(),
    oneOf = list()
  )

  # Parse each property of the event
  for (prop_name in names(event$properties)) {
    prop <- event$properties[[prop_name]]
    parsed_event$properties[[prop_name]] <- parse_property(prop, schema)
  }

  # Parse oneOf field if present
  if (!is.null(event$oneOf)) {
    parsed_event$oneOf <- lapply(event$oneOf, function(option) {
      list(
        title = get_multilingual_field(option, "title"),
        properties = lapply(option$properties, function(p) parse_property(p, schema))
      )
    })
  }

  return(parsed_event)
}


#' Parse individual property
#'
#' @param prop A list representing a property in the schema
#' @return A parsed property structure
parse_property <- function(prop, schema) {
  parsed_prop <- list(
    title = get_multilingual_field(prop, "title"),
    type = prop$type
  )

  if (!is.null(prop$allOf)) {
    # Handle allOf properties (typically used for references)
    parsed_prop$type <- "select"
    ref <- prop$allOf[[2]]$`$ref`
    if (!is.null(ref)) {
      ref_path <- strsplit(sub("^#/", "", ref), "/")[[1]]
      ref_def <- Reduce(`[[`, ref_path, schema)
      if (!is.null(ref_def$oneOf)) {
        parsed_prop$choices <- lapply(ref_def$oneOf, function(choice) {
          list(title = get_multilingual_field(choice, "title"), value = choice$const)
        })
      }
    }
  }

  if (!is.null(prop$items)) {
    # Handle array items
    parsed_prop$items <- parse_property(prop$items, schema)
  }

  if (!is.null(prop$properties)) {
    # Handle nested object properties
    parsed_prop$properties <- lapply(prop$properties, function(p) parse_property(p, schema))
  }

  if (!is.null(prop$`x-ui`)) {
    # Handle UI-specific properties
    parsed_prop$ui <- prop$`x-ui`
  }

  if (!is.null(prop$minimum)) parsed_prop$minimum <- prop$minimum
  if (!is.null(prop$maximum)) parsed_prop$maximum <- prop$maximum

  if (!is.null(prop$oneOf)) {
    parsed_prop$oneOf <- lapply(prop$oneOf, function(option) {
      list(
        title = if (!is.null(option$title)) {
          get_multilingual_field(option, "title")
        } else {
          list(en = option$const, fi = option$const, sv = option$const)
        },
        value = option$const
      )
    })
    parsed_prop$type <- "select"
  }


  return(parsed_prop)
}



#' Get multilingual field
#'
#' @param obj A list containing multilingual fields
#' @param field The base name of the field
#' @return A list of multilingual values
get_multilingual_field <- function(obj, field) {
  # Extract and return multilingual values for the given field
  list(
    en = obj[[field]],
    fi = obj[[paste0(field, "_fi")]],
    sv = obj[[paste0(field, "_sv")]]
  )
}
