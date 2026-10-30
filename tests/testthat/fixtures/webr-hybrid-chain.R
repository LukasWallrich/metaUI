# Error-handling implementation from webR 0.6.0, Shiny 1.13.0.8000.
# Keep this fixture to exercise its calling-handler semantics on native R.
webr_hybrid_chain <- function (expr, ..., catch = NULL, finally = NULL, domain = NULL, 
    replace = FALSE) 
{
    do <- function() {
        runFinally <- TRUE
        on.exit({
            if (runFinally && !is.null(finally)) finally()
        })
        catch_e <- NULL
        delayedAssign("do_catch", if (!is.null(catch)) {
            return(catch(catch_e))
        }
        else {
            stop(catch_e)
        })
        handlers <- list(error = function(e) {
            catch_e <<- e
            do_catch
        })
        classes <- names(handlers)
        .Internal(.addCondHands(classes, handlers, parent.frame(), 
            NULL, TRUE))
        result <- withVisible(force(expr))
        if (is.promising(result$value)) {
            p <- promise_chain(valueWithVisible(result), ..., 
                catch = catch, finally = finally)
            runFinally <- FALSE
            p
        }
        else {
            result <- Reduce(function(v, func) {
                if (v$visible) {
                  withVisible(func(v$value))
                }
                else {
                  withVisible(func(invisible(v$value)))
                }
            }, list(...), result)
            valueWithVisible(result)
        }
    }
    if (!is.null(domain)) {
        with_promise_domain(domain, do(), replace = replace)
    }
    else {
        do()
    }
}
