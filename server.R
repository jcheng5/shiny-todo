shinyServer(function(input, output, session) {
  
  # This is a reactive values object that will be the main
  # storage for our task data. Each task will have an entry
  # in all three vectors; "tasks" indicates the name of the
  # task; "done" is TRUE if completed, FALSE if not; and
  # "hidden" is TRUE if the task needs to be suppressed from
  # view.
  values <- reactiveValues(
    tasks = character(),
    done = logical(),
    hidden = logical()
  )
  
  # This observer handles the creation of new tasks
  observe({
    
    # The only reactive it depends on is input$newtask.
    # (Other reactive values are accessed, but only in
    # the isolate() block, so the observer won't run
    # automatically when those change.)
    
    newtask <- input$newtask
    if (is.null(newtask))
      return()  # No command entered yet
    
    isolate({
      # Add the new task and the fact that it's not done
      values$tasks <- c(values$tasks, newtask)
      values$done <- c(values$done, FALSE)
      values$hidden <- c(values$hidden, FALSE)
      
      # The renderUI block down below will make a new
      # row with new done_x and task_x inputs. Make new
      # observers to monitor each of those so we can write
      # changes in those values to our lists.
      i <- length(values$tasks)
      observe({
        isDone <- input[[sprintf("done_%s", i)]]
        if (is.null(isDone))
          return()  # The done_x input wasn't created yet
        
        # The done value changed. Update values$done.
        # Note that we have to read values$done in an
        # isolate block, otherwise it will cause an endless
        # cycle.
        isolate({
          done <- values$done
          done[[i]] <- isDone
          values$done <- done
        })
      })
      
      observe({
        task <- input[[sprintf("task_%s", i)]]
        if (is.null(task))
          return()  # The task_x input wasn't created yet

        # The task name changed. Update values$tasks.
        # Note that we have to read values$tasks in an
        # isolate block, otherwise it will cause an endless
        # cycle.
        isolate({
          tasks <- values$tasks
          tasks[[i]] <- task
          values$tasks <- tasks
        })
      })
    })
  })
  
  # Render a message indicating # of active tasks
  output$count <- renderUI({
    count <- length(which(!values$done))
    tags$p(
      tags$strong(count),
      if (count == 1) "item" else "items",
      "left"
    )
  })
  
  # Render the list of tasks as a table
  output$tasks <- renderUI({
    if (length(values$done) == 0)
      return()
    
    allowed <- switch(input$filter,
      "All" = c(TRUE, FALSE),
      "Active" = FALSE,
      "Completed" = TRUE
    )
    
    # Read values$hidden outside the isolate block
    hidden <- values$hidden
    
    isolate({
      tags$table(
        lapply(1:length(values$done), function(i) {
          # Don't show if it's hidden
          if (values$hidden[[i]])
            return()
          # Don't show if it doesn't pass the filter
          if (!(values$done[[i]] %in% allowed))
            return()
          
          # Render the row. Each row contains two inputs, so
          # that the user can modify the done state and task
          # name.
          tags$tr(
            tags$td(
              checkboxInput(sprintf("done_%s", i), NULL,
                value = values$done[[i]])
            ),
            tags$td(
              textInput(sprintf("task_%s", i), NULL,
                value = values$tasks[[i]])
            )
          )
        })
      )
    })
  })
  
  # A small observer to mark any done tasks as hidden when
  # the user hits the "Clear" button.
  observe({
    if (input$clear == 0)
      return()
    isolate({
      values$hidden <- values$hidden | values$done
    })
  })
})
