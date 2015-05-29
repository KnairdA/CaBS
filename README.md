# CaBS

… is a KISS content addressable blob storage system.

All blobs are hashed using _SHA256_ and stored in a single level prefix tree where the first two bytes of the hash determine the folder or branch name and the remaining 62 bytes determine the file name.

The `CaBS` executable offers commands for adding, retrieving and validating blobs using their hash, i.e. their content. Additionally it generates storage digest hash values summarizing a complete store.This allows for trivial comparison between multiple instances of the same store.

The goal is to develop a trivially synchronizable blob storage system that is useful both as a deduplicated storage for files one doesn't wish to name explicitly and as a foundation for higher order storage systems.  

