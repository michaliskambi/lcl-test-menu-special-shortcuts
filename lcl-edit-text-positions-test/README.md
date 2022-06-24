Test that `SelStart`, `SelLength` in LCL already account for UTF8 in Strings,
and operations that work on editbox text should account for UTF8 in Strings too,
e.g. to simulate "delete" one should do

```
Text :=
  UTF8Copy(Text, 1, SelStart) +
  UTF8SEnding(Text, SelStart + 2);
```
