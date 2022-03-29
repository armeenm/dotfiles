{ config, lib, domain, ... }:

let
  openldap = config.services.openldap.package;
  dbDir = "/var/db/ldap";
  suffix = "dc=armeen,dc=org";
in
{
  services = {
    openldap = {
      enable = false;
      urlList = [ "ldap:///" ];
      settings = {
        attrs.olcLogLevel = [ "stats" ];
        children = {
          "cn=schema".includes = [
            "${openldap}/etc/schema/core.ldif"
            "${openldap}/etc/schema/cosine.ldif"
            "${openldap}/etc/schema/inetorgperson.ldif"
          ];

          "olcDatabase={-1}frontend".attrs = {
            objectClass = "olcDatabaseConfig";
            olcDatabase = "{-1}frontend";
            olcAccess = [
              "{0}to * by dn.exact=uidNumber=0+gidNumber=0,cn=peercred,cn=external,cn=auth manage stop by & none stop"
            ];
          };

          "olcDatabase={0}config".attrs = {
            objectClass = "olcDatabaseConfig";
            olcDatabase = "{0}config";
            olcAccess = [ "{0}to * by * none break" ];
          };

          "olcDatabase={1}mdb".attrs = {
            objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
            olcDatabase = "{1}mdb";
            olcDbDirectory = dbDir;
            olcDbIndex = [
              "objectClass eq"
              "uid pres,eq"
              "cn pres,eq"
              "sn pres,eq,subany"
            ];
            olcSuffix = suffix;
            olcAccess = [ "{0}to * by * read break" ];
          };
        };
      };
    };
  };
}
