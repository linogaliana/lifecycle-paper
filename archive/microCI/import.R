httr::set_config(httr::config(ssl_verifypeer = 0L))

options(download.file.method="curl")
options(download.file.extra = paste0("--noproxy ", "https://minio.stable.innovation.insee.eu"))


## Sys.setenv("AWS_SESSION_TOKEN" ="",
##           "AWS_ACCESS_KEY_ID" = "minio",
##           "AWS_SECRET_ACCESS_KEY" = "minio123",
##           "AWS_DEFAULT_REGION" = "us-east-1",
##           "AWS_S3_ENDPOINT" = "minio.alpha.innovation.insee.eu",
##           "AWS_EXPIRATION" = format(Sys.time()+3600*23, "%Y-%M-%dT%TZ"),
##           "LIBCURL_BUILD"="winssl",
##           "SSH_ASKPASS" = "rpostback-askpass")

minior::set_minio_shared()


download.file("http://minio.alpha.innovation.insee.eu/minio/w3crk9/microsimulation/Destinie.zip",
dest = "/home/Destinie.zip")
download.file("http://minio.alpha.innovation.insee.eu/minio/w3crk9/microsimulation/Destinie.zip",
dest = "/home/Enquete Patrimoine.zip")