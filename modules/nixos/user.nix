{ config, user, ... }:

{
  age.secrets."${user.login}-pw".file = ../../secrets/${user.login}-pw.age;
  users.users."${user.login}".hashedPasswordFile = config.age.secrets."${user.login}-pw".path;
}
