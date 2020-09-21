if (file.exists('~/.Rprofile')) sys.source('~/.Rprofile', envir = environment())

if (!file.exists("~/Destinie.zip")) utils::download.file("https://minio.stable.innovation.insee.eu/groupe-788/Destinie.zip", dest = "~/Destinie.zip")
if (!file.exists("~/Enquete Patrimoine.zip")) utils::download.file("https://minio.stable.innovation.insee.eu/groupe-788/Enquete%20Patrimoine.zip", dest = "~/Enquete Patrimoine.zip")

system("git submodule update --init --recursive")
