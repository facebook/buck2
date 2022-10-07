{
  if ($1 ~ /^unix|windows|target_/) {
    st = index($0, "=")
    if (st > 0) {
      key = substr($0, 0, st-1)
      val = substr($0, st+1)
      gsub("\"", "", val)
    } else {
      key = $0
      val = ""
    }
    key="CARGO_CFG_"toupper(key)
    if (key in cfgs) {
      cfgs[key] = cfgs[key]","val
    } else {
      cfgs[key] = val
    }
  }
} END {
  for (key in cfgs) {
    if (cfgs[key] == "") {
      print key"=1";
    } else {
      print key"="cfgs[key];
    }
  }
}
