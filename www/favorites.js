// 5 Buckets Impact Dashboard — browser-local favorites (localStorage)

var FAV_STORAGE_KEY = "5buckets_favorites_v1";

function favReadAll() {
  try {
    var raw = localStorage.getItem(FAV_STORAGE_KEY);
    if (!raw) return [];
    var parsed = JSON.parse(raw);
    return Array.isArray(parsed) ? parsed : [];
  } catch (e) {
    console.warn("favorites read failed", e);
    return [];
  }
}

function favWriteAll(arr) {
  localStorage.setItem(FAV_STORAGE_KEY, JSON.stringify(arr));
}

function favAdd(item) {
  var list = favReadAll();
  list = list.filter(function(x) { return x.id !== item.id; });
  list.push(item);
  favWriteAll(list);
  return list.length;
}

function favRemove(id) {
  var list = favReadAll().filter(function(x) { return x.id !== id; });
  favWriteAll(list);
  return list.length;
}

function favExportJson() {
  return JSON.stringify(favReadAll(), null, 2);
}

function favImportJson(jsonStr) {
  var parsed = JSON.parse(jsonStr);
  if (!Array.isArray(parsed)) throw new Error("Expected JSON array");
  favWriteAll(parsed);
  return parsed.length;
}
