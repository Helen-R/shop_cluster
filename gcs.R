mylib("googleCloudStorageR")
default.bucket <- "helen-ml-4standard"
Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
           "GCS_DEFAULT_BUCKET" = default.bucket)
gcs_auth()
gcs_global_bucket(default.bucket)
objs <- gcs_list_objects()
idx <- 8
target <- objs$name[[idx]]
print(target)
gcs_get_object(target, saveToDisk = target, overwrite = TRUE)
system(sprintf("unzip %s -d data/raw_input/", target))

# gcs_upload(sprintf("%s.zip", suffix), name = sprintf("%s-1.zip", suffix))
