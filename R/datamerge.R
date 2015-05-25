#' Merge data frames
#'
#' This function combines data from data frames in falling priority. That is
#' whenever the first data frame is missing a row, column and/or value it will
#' be taken from the first following data frame which does have it.
#'
#' The task the function was written to solve is to merge a collection of
#' inconsistent versioned xls files. To do this import all the files using
#' \code{\link[utils]{read.csv}} or similar, make sure row and column names are
#' consistent (otherwise they wont be recognized as matching), run
#' \code{version.merge} and manually take care of the problems reported.
#'
#' @param ... A list of data frames to be merged with falling priority.
#' @param add.cols Logical indicating if columns missing in frames with higher
#'   priority should be added from frames with lower priority.
#' @param add.rows Logical indicating if rows missing in frames with higher
#'   priority should be added from frames with lower priority.
#' @param add.values Logical indicating if missing values in frames with higher
#'   priority should be imputed from frames with lower priority.
#' @param verbose Logical indicating if a summary should be displayed.
#' @return A single merged data frame.
#' @examples
#' # Make 3 similar data frames
#' frames <- lapply(1:3, function(i){
#'     d <- data.frame(matrix(runif(60), 10, 6))
#'     d[[sample(6, 1)]][sample(10, 5)] <- NA
#'     rownames(d) <- paste("obj_", sample(20, 10), sep="")
#'     names(d) <- c(paste("feat_", sort(sample(8, 6)), sep=""))
#'     return(d)
#' })
#' merged.frame <- version.merge(frames[[1]], frames[[2]], frames[[3]],
#'     add.values=TRUE, verbose=TRUE)
#'
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @import xtermStyle
#' @export
version.merge <- function(..., add.cols=TRUE, add.rows=TRUE, add.values=FALSE, verbose=TRUE){
    frames <- list(...)
    frame.names <- sapply(match.call()[-1], deparse)[1:length(frames)]
    pal <- if(options("color.scheme") == "dark on light") "Dark2" else "Set2"
    for(i in 1:length(frame.names)){
        frame.names[i] <- sprintf("`%s` %s", style(frame.names[i], fg=xterm.pal()[[pal]][i]),
            sprintf("#%i", i))
    }
    
    if(any(!sapply(frames, is.data.frame)))
        stop("Data must be specified as data frames")
    if(length(frames) == 1 || (!add.cols && !add.rows && !add.values)){
        if(verbose) cat("Nothing to be done.\n")
        return(frames[[1]])
    }

    row.counts <- rep(NA, length(frames))
    col.history <- list()
    warn <- FALSE
    
    row.counts[1] <- nrow(frames[[1]])
    for(c in names(frames[[1]])) col.history[[c]] <- sprintf("Origin: %s", frame.names[1])
    
    # Merge
    for(i in 2:length(frames)){
        new.rows <- rep(FALSE, nrow(frames[[1]]))
        if(add.rows){
            missing.rows <- rownames(frames[[i]])[!rownames(frames[[i]]) %in% rownames(frames[[1]])]
            row.counts[i] <- length(missing.rows)
            if(row.counts[i] > 0){
                frames[[1]][missing.rows, ] <- NA
                new.rows <- c(new.rows, rep(TRUE, row.counts[i]))
            }
        }

        if(add.values || sum(new.rows) > 0){
            for(c in names(frames[[1]])){
                if(c %in% names(frames[[i]])){
                    idx <- if(add.values) is.na(frames[[1]][[c]]) else new.rows
                    if(sum(idx) > 0){
                        val <- frames[[i]][rownames(frames[[1]])[idx], c]
                        if(sum(!is.na(val)) > 0){
                            if(sum(!is.na(val)) > sum(new.rows))
                                col.history[[c]] <- c(col.history[[c]], sprintf("Imputed %i values from %s",
                                    sum(!is.na(val)), frame.names[i]))
                            
                            # Check datatypes
                            c.1 <- class(frames[[1]][[c]])
                            c.i <- class(frames[[i]][[c]])
                            if(!identical(c.1, c.i)){
                                warn <- TRUE
                                col.history[[c]] <- c(col.history[[c]], style(fg="red", sprintf("Class missmatch: %s vs. %s", c.1, c.i)))
                                if(c.1 %in% c("numeric", "integer") && c.i %in% c("numeric", "integer")){
                                    frames[[1]][[c]] <- as.numeric(frames[[1]][[c]])
                                    col.history[[c]] <- c(col.history[[c]], style(fg="red", sprintf("Converted to numeric")))
                                } else {
                                    frames[[1]][[c]] <- as.character(frames[[1]][[c]])
                                    col.history[[c]] <- c(col.history[[c]], style(fg="red", sprintf("Converted to character")))
                                }
                            }
                            if(c.1 == "factor"){
                                if(!identical(levels(frames[[1]][[c]]), levels(frames[[i]][[c]]))){
                                    col.history[[c]] <- c(col.history[[c]],
                                        style(fg="red", "Factor level missmatch"),
                                        style(fg="red", "Appended missing levels"))
                                    if(is.ordered(frames[[1]][[c]]) || is.ordered(frames[[i]][[c]])){
                                        col.history[[c]] <- c(col.history[[c]],
                                            style(fg="red", "Level order lost"))
                                    }
                                    new.levels <- unique(c(levels(frames[[1]][[c]]), levels(val)))
                                    frames[[1]][[c]] <- factor(as.character(frames[[1]][[c]]), levels=new.levels)
                                    val <- factor(as.character(val), levels=new.levels)
                                }
                            }
                            frames[[1]][idx, c] <- val
                        }
                    }
                }
            }
        }

        if(add.cols){
            missing.cols <- names(frames[[i]])[!names(frames[[i]]) %in% names(frames[[1]])]
            if(length(missing.cols) > 0){
                shared.rows <- unlist(sapply(rownames(frames[[1]]), function(r) which(r == rownames(frames[[i]]))))
                for(c in missing.cols){
                    col.history[[c]] <- sprintf("Origin: %s", frame.names[i])
                    # Add the new column but not the data
                    if(is.factor(frames[[i]][[c]])){
                        # Factors need special care otherwise will the resulting column be of integer type
                        frames[[1]][[c]] <- factor(NA, levels=levels(frames[[i]][[c]]))
                        if(is.ordered(frames[[i]][[c]]))
                            frames[[1]][[c]] <- as.ordered(frames[[1]][[c]])
                    } else {
                        frames[[1]][[c]] <- NA
                    }
                    frames[[1]][names(shared.rows), c] <- frames[[i]][[c]][shared.rows]
                }
            }
        }
    }

    if(verbose){
        if(add.rows){
            cat("Rows:")
            indent.size <- 7 + max(nchar(as.character(row.counts)))
            for(i in 1:length(frames))
                cat(sprintf("%s%i from %s\n",
                    paste(rep(" ", indent.size - 5*(i==1) - nchar(as.character(row.counts[i]))), collapse=""), row.counts[i],
                    frame.names[i]))
        }
        if(add.cols || add.values){
            cat("\nColumns:\n")
            indent.size <- max(nchar(names(frames[[1]]))) + 2
            for(i in 1:length(col.history)){
                cat(style.auto(frames[[1]][[i]], names(col.history)[i]))
                for(j in 1:length(col.history[[i]])){
                    cat(sprintf("%s%s\n", paste(rep(" ", indent.size - nchar(names(col.history)[i])*(j==1)), collapse=""), col.history[[i]][j]))
                }
            }
        }
    } else if(warn)
        warning("Problems were encountered when merging, run with verbose=TRUE for details.")

    return(frames[[1]])
}

#' Cleaning factors
#'
#' Removes empty factor levels and converts to numeric or character where
#' appropriate.
#'
#' @param df A data frame to operate on.
#' @param clear.blank Whether to replace blank factor levels (i.e. "") with
#'   \code{NA}.
#' @param drop.levels Whether to drop unused factor levels.
#' @param drop.columns Whether to drop empty columns (all \code{NA}).
#' @param fac2num Whether to convert factors with numeric levels to
#'   numeric.
#' @param char2fac A threshold for converting characters into factors. If
#'   the percentage of unique values is less than this value the character is
#'   converted. \code{NA} is omitted.
#' @param verbose Whether to print a summary of changes.
#' @return A new data frame with modified factor variables.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
clean.factors <- function(df, clear.blank=TRUE, drop.levels=TRUE, drop.columns=TRUE,
                          fac2num=TRUE, char2fac=.5, verbose=TRUE){
    width <- max(sapply(names(df), nchar))
    for(name in names(df)){
        orig.class <- df[1,name]
        msg <- NULL
        if(drop.columns && all(is.na(df[[name]]))){
            df[[name]] <- NULL
            msg <- c(msg, style.auto(NULL, "Empty, dropped."))
        } else if(is.factor(df[[name]])){
            df[[name]][grepl("^\\s*$", df[[name]])] <- NA # Replace empty factor-level with NA

            if(clear.blank && "" %in% levels(df[[name]])){
                df[[name]][df[[name]] == ""] <- NA
                msg <- c(msg, style.auto(factor(), "Cleared blank factor level."))
            }
            if(drop.levels && any(table(df[[name]]) == 0)){
                df[[name]] <- factor(df[[name]], ordered=is.ordered(df[[name]]))
                msg <- c(msg, style.auto(factor(), "Dropped unused factor levels."))
            }

            if(all(grepl("^\\d+(\\.\\d+)?$", levels(df[[name]])))){
                # All levels are numeric and the data is probably best described that way
                df[[name]] <- as.numeric(df[[name]])
                msg <- c(msg, style.auto(0, "Converted to numeric."))
            } else {
                # If there are as many levels as values this variable is probably
                # better represented as character.
                if(length(levels(df[[name]])) == sum(!is.na(df[[name]]))){
                    df[[name]] <- as.character(df[[name]])
                    msg <- c(msg, style.auto("", "Converted to character."))
                }
            }
        } else if((is.na(char2fac) || is.null(char2fac)) && is.character(df[[name]])){
            if(length(unique(df[[name]])) / sum(!is.na(df[[name]]) & df[[name]] != "") < char2fac){
                if(clear.blank) df[[name]][df[[name]] == ""] <- NA
                df[[name]] <- factor(df[[name]])
                msg <- c(msg, style.auto(factor(), sprintf("Converted to factor (%i levels).", length(levels(df[[name]])))))
            }
        }
       
        if(is.null(msg)) msg <- style.auto(df[1,name], "No change.")

        if(verbose){
            cat(sprintf("%s%s%s\n",
                        style.auto(orig.class, sprintf(sprintf("%%%is", width), name)),
                        ": ",
                        paste(msg, collapse=sprintf(sprintf("\n%%%is", width+2), " "))))
        }
    }
    return(df)
}

