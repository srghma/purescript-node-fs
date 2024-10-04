export const closeImpl = (dir, callback) => dir.close(callback);
export const closeSyncImpl = (dir) => dir.close();
export const path = (dir) => dir.path;
export const readImpl = (dir, callback) => dir.read(callback);
export const readSyncImpl = (dir) => dir.read();
