open Core.Std
open Ctypes
open PosixTypes
open Foreign


type struct_passwd
let struct_passwd : struct_passwd structure typ = structure "passwd"
let pw_name   = field struct_passwd "pw_name"   string
let pw_passwd = field struct_passwd "pw_passwd" string
let pw_uid    = field struct_passwd "pw_uid"    uid_t
let pw_gid    = field struct_passwd "pw_gid"    gid_t
let pw_change = field struct_passwd "pw_change" time_t
let pw_class  = field struct_passwd "pw_class"  string
let pw_gecos  = field struct_passwd "pw_gecos"  string
let pw_dir    = field struct_passwd "pw_dir"    string
let pw_shell  = field struct_passwd "pw_shell"  string
let pw_expire = field struct_passwd "pw_expire" time_t
let pw_fields = field struct_passwd "pw_fields" int
let () = seal struct_passwd

(*
    struct passwd *getpwuid(uid_t uid);
 *)
let getpwuid =
  foreign "getpwuid" (uid_t @-> returning_checking_errno (ptr struct_passwd))

let getusername uid =
  let passwd = getpwuid uid in
  getf (!@ passwd) pw_name


type struct_group
let struct_group : struct_group structure typ = structure "group"
let gr_name   = field struct_group "gr_name"   string
let gr_passwd = field struct_group "gr_passwd" string
let gr_gid    = field struct_group "gr_gid"    gid_t
let gr_mem    = field struct_group "gr_mem"    (ptr string)
let () = seal struct_group

(*
   struct group *getgrgid(gid_t gid);
 *)
let getgrgid =
  foreign "getgrgid" (gid_t @-> returning_checking_errno (ptr struct_group))

let getgroupname gid =
  let group = getgrgid gid in
  getf (!@ group) gr_name
