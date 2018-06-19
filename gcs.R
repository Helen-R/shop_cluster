mylib("googleCloudStorageR")
default.bucket <- "helen-ml-4standard"
Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
           "GCS_DEFAULT_BUCKET" = default.bucket)
gcs_auth()
gcs_global_bucket(default.bucket)
objs <- gcs_list_objects()
objs$name
idx <- 11
for (idx in c(8:10, 12:13)) {
    target <- objs$name[[idx]]
    print(target)
    gcs_get_object(target, saveToDisk = target, overwrite = TRUE)
    file.rename(target, file.path("data/raw_input", target))
}

for (idx in 14:16) {
    target <- objs$name[[idx]]
    print(target)
    system(sprintf("zip %s.zip %s", target, target))
    gcs_upload(sprintf("%s.zip", target), name = sprintf("%s.zip", target))
}
idx <- 17
target <- objs$name[[idx]]
print(target)
gcs_upload(sprintf("%s.zip", target), name = sprintf("%s.zip", target))
