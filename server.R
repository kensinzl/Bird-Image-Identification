server = function(input, output) {
    
    # show the upload image
    output$output_image = renderImage(
        expr ={
            # check the input$input_image is truthy
            # If any of the given values is not truthy, 
            # the operation is stopped by raising a "silent" exception (not logged by Shiny, nor displayed in the Shiny app's UI).
            # 
            # If do not add, then the first time, UI would complain there is no upload image path
            req(input$input_image)
            list(src = input$input_image$datapath, contentType=input$input_image$type, width = 400)
        }, 
        # the image is temp file because you upload from your local
        # but this temp file would be used for the later prediction, so I need to keep it at the browser, DO NOT DELETE.
        deleteFile = FALSE
    )
    
    # load the image for the model prediction
    image = reactive({
        image_load(input$input_image$datapath, target_size = target_size[1:2])
    })
    
    # model prediction
    prediction = reactive({
        # here I can replace by if(is.null(input$input_image)){return(NULL)}
        req(input$input_image)
        x = image_to_array(image())
        x = array_reshape(x, c(1, dim(x)))
        x = x/255
        pred = model %>% predict(x)
        # transpose pred from row into column
        pred = data.frame("Bird" = label_list, "Prediction" = t(pred))
        pred = pred[order(pred$Prediction, decreasing=T), ][1:5,]
        pred$Prediction = sprintf("%.2f %%", 100*pred$Prediction)
        pred
    })
    
    # show the prediction result
    output$text = renderTable({
        prediction()
    })
    
    # to be honest, not necessary to have this
    output$warntext = renderText({
        req(input$input_image)
        if(as.numeric(substr(prediction()[1,2], 1, 4)) >= 30) {
            return(NULL)
        }
        warntext = "Warning: I am not sure about this bird!"
        warntext
    })
}