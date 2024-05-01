#' Clean IRS Data for Shiny App
#'
#' Sample of cleaned up IRS Data product after running load_soi() and clean_soi()
#' Only includes 2020 for CT
#'
#' @format A data frame with 2033904 rows and 36 variables:
#' \describe{
#'   \item{zipcode}{zipcode, chr}
#'   \item{year}{year, int}
#'   \item{state}{state, chr}
#'   \item{agi_level}{five income levels, chr}
#'   \item{agi_level_2}{five income levels, chr}
#'   \item{a00100}{Adjusted Gross Income, num}
#'   \item{a00200}{Salary & Wages Amount, num}
#'   \item{n00200}{Returns with Salary & Wages, num}
#'   \item{total_tax}{Total tax paid, num}
#'   \item{taxpayers}{Number of Returns with Income Tax, num}
#'   \item{n1}{number of taxpayers, int}
#'   \item{n2}{Number Exemptions, num}
#'   \item{numdep}{Dependent Exemptions, num}
#'   \item{a01000}{Net Capital Gain, num}
#'   \item{n01000}{Returns with Net Capital Gain, num}
#'   \item{a02300}{Unemployment Comp, num}
#'   \item{n02300}{Returns with Unemployment Comp, num}
#'   \item{a59660}{Earned Income Credit Amount, num}
#'   \item{n59660}{Returns with earned income credit, num}
#'   \item{a09750}{Healthcare Individual Resp. Payment, num}
#'   \item{n09750}{Number of returns with health care, num}
#'   \item{a04470}{Total Itemized Deductions Amount, num}
#'   \item{a02500}{Taxable Social Security benefits, num}
#'   \item{n02500}{Returns with Soc Security, num}
#'   \item{a18425}{State & Local Taxes Amount, num}
#'   \item{n18425}{Returns with State & Local Taxes, num}
#'   \item{a18450}{State & Local General Sales Tax Amount, num}
#'   \item{n18450}{Returns with State & Local General Sales Tax, num}
#'   \item{a18500}{Real Estate Taxes Amount, num}
#'   \item{n18500}{Returns with Real Estate Taxes, num}
#'   \item{a18800}{Personal Property Tax amount, num}
#'   \item{n18800}{Returns with Personal Property Tax, num}
#'   \item{a19300}{Mortgage Interest Paid Amount, num}
#'   \item{n19300}{Returns with Mortgage Interest Paid, num}
#'   \item{a00600}{Ordinary Dividends Amount, num}
#'   \item{n00600}{Returns with Ordinary Dividends, num}
#'   \item{a00300}{Taxable Interest Amount, num}
#'   \item{n00300}{Returns with Taxable Interest, num}
#'   \item{a00900}{Business or professional net income, num}
#'   \item{n00900}{Returns with Business or professional, num}
#'   \item{a19700}{Contributions amount, num}
#'   \item{n19700}{Returns with Contributions, num}
#'   \item{a26270}{Partnership/S-corp net income, num}
#'   \item{n26270}{Returns Partnership/S-corp, num}
#'   \item{a01700}{Taxable pensions and annuities, num}
#'   \item{n01700}{Returns with Taxable pensions and annuities, num}
#'   \item{a09600}{Alternative minimum tax amount, num}
#'   \item{n09600}{Returns with Alternative minimum tax amount, num}
#'   \item{a11000}{Earned Income Credit amount, num}
#'   \item{n11000}{Returns with Earned Income Credit amount, num}
#'   \item{a85300}{Net investment income tax amount, num}
#'   \item{n85300}{Returns with Net investment income tax, num}
#'
#' }
#' @source \url{https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi/}
#'
"irs_app_data"


#' Data dictionary
#'
#' @format A data frame with 968 rows and 3 variables:
#' \describe{
#'   \item{name}{IRS SOI Field, chr}
#'   \item{desc}{Field Description, chr}
#'   \item{year}{year, chr}
#' }
#' @source \url{https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi/}
#'
"data_dict"


#' Zipcodes
#'
#' @format A data frame with 33144 rows and 3 variables:
#' \describe{
#'  \item{zipcode}{Zipcode, chr}
#'  \item{county}{County, chr}
#'  \item{major_city}{Major City, chr}
#'  \item{land_area_in_sqmi}{Land Area, num}
#'  \item{lat}{Latitude, num}
#'  \item{lng}{Longitude, num}
#'  }
#'  @source \url{https://www.unitedstateszipcodes.org/zip-code-database/}
#'
"zipcodes"
