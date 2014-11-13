open Core.Std
open Ctypes
open PosixTypes
open Foreign


type struct_passwd
let struct_passwd : struct_passwd structure typ = structure "passwd"
let pw_name   = field struct_passwd "pw_name"   (ptr char)
let pw_passwd = field struct_passwd "pw_passwd" (ptr char)
let pw_uid    = field struct_passwd "pw_uid"    uid_t
let pw_gid    = field struct_passwd "pw_gid"    gid_t
let pw_change = field struct_passwd "pw_change" time_t
let pw_class  = field struct_passwd "pw_class"  (ptr char)
let pw_gecos  = field struct_passwd "pw_gecos"  (ptr char)
let pw_dir    = field struct_passwd "pw_dir"    (ptr char)
let pw_shell  = field struct_passwd "pw_shell"  (ptr char)
let pw_expire = field struct_passwd "pw_expire" time_t
let pw_fields = field struct_passwd "pw_fields" int
let () = seal struct_passwd

(*
    struct passwd *getpwuid(uid_t uid);
 *)
let getpwuid =
  foreign "getpwuid" (uid_t @-> returning (ptr struct_passwd))


type struct_group
let struct_group : struct_group structure typ = structure "group"
let gr_name   = field struct_group "gr_name"   (ptr char)
let gr_passwd = field struct_group "gr_passwd" (ptr char)
let gr_gid    = field struct_group "gr_gid"    gid_t
let gr_mem    = field struct_group "gr_mem"    (ptr (ptr char))
let () = seal struct_group

(*
   struct group *getgrgid(gid_t gid);
 *)
let getgrgid =
  foreign "getgrgid" (gid_t @-> returning (ptr struct_group))
