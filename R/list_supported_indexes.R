#' List Supported Indexes
#'
#' This function returns a tibble containing information about supported financial indexes.
#' Each index is associated with a URL that points to a CSV file containing the holdings of the index.
#' Additionally, each index has a corresponding `skip` value, which indicates the number of lines
#' to skip when reading the CSV file.
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{index}{The name of the financial index (e.g., "DAX", "S&P 500").}
#'   \item{url}{The URL to the CSV file containing the holdings data for the index.}
#'   \item{skip}{The number of lines to skip when reading the CSV file.}
#' }
#'
#' @export
#'
#' @examples
#' supported_indexes <- list_supported_indexes()
#' print(supported_indexes)
#'
list_supported_indexes <- function() {
  tribble(
    ~index                                                                                                                                                                                    ,
    ~url                                                                                                                                                                                      ,
    ~skip                                                                                                                                                                                     ,
    "DAX"                                                                                                                                                                                     ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251464/ishares-dax-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=DAXEX_holdings&dataType=fund"                         ,
                                                                                                                                                                                            2 ,
    "EURO STOXX 50"                                                                                                                                                                           ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251783/ishares-euro-stoxx-50-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXW1_holdings&dataType=fund"                ,
                                                                                                                                                                                            2 ,
    "Dow Jones Industrial Average"                                                                                                                                                            ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251770/ishares-dow-jones-industrial-average-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXI3_holdings&dataType=fund" ,
                                                                                                                                                                                            2 ,
    "Russell 1000"                                                                                                                                                                            ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239707/ishares-russell-1000-etf/1495092304805.ajax?fileType=csv&fileName=IWB_holdings&dataType=fund"                       ,
                                                                                                                                                                                            9 ,
    "Russell 2000"                                                                                                                                                                            ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239710/ishares-russell-2000-etf/1495092304805.ajax?fileType=csv&fileName=IWM_holdings&dataType=fund"                       ,
                                                                                                                                                                                            9 ,
    "Russell 3000"                                                                                                                                                                            ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239714/ishares-russell-3000-etf/1495092304805.ajax?fileType=csv&fileName=IWV_holdings&dataType=fund"                       ,
                                                                                                                                                                                            9 ,
    "S&P 100"                                                                                                                                                                                 ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239723/ishares-sp-100-etf/1495092304805.ajax?fileType=csv&fileName=OEF_holdings&dataType=fund"                             ,
                                                                                                                                                                                            9 ,
    "S&P 500"                                                                                                                                                                                 ,
    "https://www.ishares.com/de/privatanleger/de/produkte/253743/ishares-sp-500-b-ucits-etf-acc-fund/1478358465952.ajax?fileType=csv&fileName=SXR8_holdings&dataType=fund"                    ,
                                                                                                                                                                                            2 ,
    "Nasdaq 100"                                                                                                                                                                              ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251896/ishares-nasdaq100-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXXT_holdings&dataType=fund"                    ,
                                                                                                                                                                                            2 ,
    "FTSE 100"                                                                                                                                                                                ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251795/ishares-ftse-100-ucits-etf-inc-fund/1478358465952.ajax?fileType=csv&fileName=IUSZ_holdings&dataType=fund"                    ,
                                                                                                                                                                                            2 ,
    "MSCI World"                                                                                                                                                                              ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251882/ishares-msci-world-ucits-etf-acc-fund/1478358465952.ajax?fileType=csv&fileName=EUNL_holdings&dataType=fund"                  ,
                                                                                                                                                                                            2 ,
    "Nikkei 225"                                                                                                                                                                              ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/253742/ishares-nikkei-225-ucits-etf/1495092304805.ajax?fileType=csv&fileName=CSNKY_holdings&dataType=fund"                 ,
                                                                                                                                                                                            2 ,
    "TOPIX"                                                                                                                                                                                   ,
    "https://www.blackrock.com/jp/individual-en/en/products/279438/fund/1480664184455.ajax?fileType=csv&fileName=1475_holdings&dataType=fund"                                                 ,
                                                                                                                                                                                            2 ,
    "STOXX Europe 600"                                                                                                                                                                        ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251931/ishares-stoxx-europe-600-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXSA_holdings&dataType=fund"             ,
                                                                                                                                                                                            2 ,
    "MDAX"                                                                                                                                                                                    ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251845/ishares-mdax-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXS3_holdings&dataType=fund"                         ,
                                                                                                                                                                                            2 ,
    "TecDAX"                                                                                                                                                                                  ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251948/ishares-tecdax-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXS2_holdings&dataType=fund"                       ,
                                                                                                                                                                                            2 ,
    "MSCI Emerging Markets"                                                                                                                                                                   ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251857/ishares-msci-emerging-markets-ucits-etf-inc-fund/1478358465952.ajax?fileType=csv&fileName=IQQE_holdings&dataType=fund"       ,
                                                                                                                                                                                            2 ,
    "MSCI Europe"                                                                                                                                                                             ,
    "https://www.ishares.com/de/privatanleger/de/produkte/251860/ishares-msci-europe-ucits-etf-inc-fund/1478358465952.ajax?fileType=csv&fileName=IQQY_holdings&dataType=fund"                 ,
                                                                                                                                                                                            2 ,
    "MSCI ACWI"                                                                                                                                                                               ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239600/ishares-msci-acwi-etf/1495092304805.ajax?fileType=csv&fileName=ACWI_holdings&dataType=fund"                         ,
                                                                                                                                                                                            9 ,
    "S&P SmallCap 600"                                                                                                                                                                        ,
    "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239774/ishares-sp-smallcap-600-etf/1495092304805.ajax?fileType=csv&fileName=IJR_holdings&dataType=fund"                    ,
                                                                                                                                                                                            9
  )
}
