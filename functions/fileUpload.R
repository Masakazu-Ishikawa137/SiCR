fileUpload <- function(input, myReactives) {
    myReactives$h5_path <- input$h5$datapath
    myReactives$tcr_path <- input$tcr$datapath
    myReactives$bcr_path <- input$bcr$datapath

    if (is.null(myReactives$h5_path)) {
        myReactives$h5_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000.h5"
        myReactives$tcr_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000_t.csv"
        myReactives$bcr_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000_b.csv"
    }
    return(myReactives)
}
