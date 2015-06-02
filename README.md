# CaBS

… is a KISS content addressable blob storage system.

All blobs are hashed using _SHA256_ and stored in a single level prefix tree where the first two bytes of the hash determine the folder or branch name and the remaining 62 bytes determine the file name.

The `CaBS` executable offers commands for adding, retrieving and validating blobs using their hash, i.e. their content. Additionally it generates storage digest hash values summarizing a complete store.This allows for trivial comparison between multiple instances of the same store.

The goal is to develop a trivially synchronizable blob storage system that is useful both as a deduplicated storage for files one doesn't wish to name explicitly and as a foundation for higher order storage systems.  

## Example usage

```
> echo "Hello world!" | cabs add
0b/a904eae8773b70c75333db4de2f3ac45a8ad4ddba1b242f0b3cfc199391dd8
> cat (cabs get "0ba904eae8773b70c75333db4de2f3ac45a8ad4ddba1b242f0b3cfc199391dd8")
Hello world!
> echo "Digest test" | cabs add
25/886f4eb2f10ed08b315b068221f1d9bc64d23b4f84fd54ca8ab0169f9ce207
> cabs ls-blobs
0ba904eae8773b70c75333db4de2f3ac45a8ad4ddba1b242f0b3cfc199391dd8
25886f4eb2f10ed08b315b068221f1d9bc64d23b4f84fd54ca8ab0169f9ce207
> cabs digest
75e72b213bd9f1aecbb146a40f5f6f13144bd427d098ce813feb8a59e8339ff7
```

CaBS `synchronize` allows two-way syncing of multiple arbitrary storage instances at one go.
