#' TRIAL vrule_lonlat_in_shape
#' @name vrule_lonlat_in_shape
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_lonlat_in_shape <- R6Class(
  "vrule_lonlat_in_shape",
  inherit = vrule_abstract_complex,

  private = list(
    category = "Space",
    name = "LonLatInShape",
    shape = NULL
  ),

  public = list(
    lon_field = NA,
    lat_field = NA,
    na_allowed = FALSE,
    crs = 4326,

    initialize = function(shape,
                          lon_field = "long",
                          lat_field = "lat",
                          na_allowed = FALSE,
                          crs = 4326,
                          ...) {
      super$initialize(...)

      if (inherits(shape, "character")) {
        shape <- sf::st_read(shape, quiet = TRUE)
      }

      if (!inherits(shape, "sf") && !inherits(shape, "sfc")) {
        stop("`shape` must be an sf/sfc object or a path readable by sf::st_read()")
      }

      private$shape <- sf::st_as_sf(shape)
      self$lon_field <- lon_field
      self$lat_field <- lat_field
      self$na_allowed <- na_allowed
      self$crs <- crs
    },

    validate = function(value, row, ...) {
      rep <- super$validate(value, row, ...)

      lon <- row[[self$lon_field]]
      lat <- row[[self$lat_field]]

      if (is.null(lon) || is.null(lat) || is.na(lon) || is.na(lat)) {
        if (!self$na_allowed) {
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = self$getType(),
            message = sprintf(
              "Longitude/latitude values are missing in fields '%s' and '%s'",
              self$lon_field, self$lat_field
            )
          )
        }
        return(rep)
      }

      lon_num <- suppressWarnings(as.numeric(lon))
      lat_num <- suppressWarnings(as.numeric(lat))

      if (is.na(lon_num) || is.na(lat_num)) {
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf(
            "Longitude/latitude values (%s, %s) are not numeric",
            lon, lat
          )
        )
        return(rep)
      }

      pt <- sf::st_as_sf(
        data.frame(lon = lon_num, lat = lat_num),
        coords = c("lon", "lat"),
        crs = self$crs
      )

      if (!is.na(sf::st_crs(private$shape))) {
        pt <- sf::st_transform(pt, sf::st_crs(private$shape))
      }

      inside <- lengths(sf::st_intersects(pt, private$shape)) > 0

      if (!inside) {
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf(
            "Point (%s, %s) lies outside the reference shape",
            lon, lat
          )
        )
      }

      return(rep)
    }
  )
)



#' vrule_coordinate
#' @name vrule_coordinate
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_coordinate <- R6::R6Class(
  "vrule_coordinate",
  inherit = vrule_numeric,

  private = list(
    category = "Space",
    name = "Coordinate"
  ),

  public = list(
    coordinate_type = NA_character_,

    initialize = function(coordinate_type = c("latitude", "longitude"),
                          na_allowed = FALSE,
                          ...) {
      coordinate_type <- match.arg(coordinate_type)

      super$initialize(na_allowed = na_allowed, ...)

      self$coordinate_type <- coordinate_type
    },

    validate = function(value, ...) {
      rep <- super$validate(value, ...)

      if (nrow(rep$report) == 0) {

        if (is.na(value)) {
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = self$getType(),
            message = sprintf("%s value is missing", self$coordinate_type)
          )
        } else if (self$coordinate_type == "latitude") {
          if (value < -90 || value > 90) {
            rep <- create_vrule_report(
              valid = FALSE,
              category = self$getCategory(),
              rule = self$getName(),
              type = self$getType(),
              message = sprintf(
                "Latitude value %s is outside valid range [-90, 90]",
                value
              )
            )
          }
        } else if (self$coordinate_type == "longitude") {
          if (value < -180 || value > 180) {
            rep <- create_vrule_report(
              valid = FALSE,
              category = self$getCategory(),
              rule = self$getName(),
              type = self$getType(),
              message = sprintf(
                "Longitude value %s is outside valid range [-180, 180]",
                value
              )
            )
          }
        }
      }

      return(rep)
    }
  )
)