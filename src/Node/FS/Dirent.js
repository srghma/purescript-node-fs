export { inspect as showDirentObj } from "util";

export const isBlockDevice = dirent => dirent.isBlockDevice();
export const isCharacterDevice = dirent => dirent.isCharacterDevice();
export const isDirectory = dirent => dirent.isDirectory();
export const isFIFO = dirent => dirent.isFIFO();
export const isFile = dirent => dirent.isFile();
export const isSocket = dirent => dirent.isSocket();
export const isSymbolicLink = dirent => dirent.isSymbolicLink();
export const nameImpl = dirent => dirent.name;
export const parentPath = dirent => dirent.parentPath;
