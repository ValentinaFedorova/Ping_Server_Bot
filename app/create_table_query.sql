CREATE TABLE UrlState (
       site_url nvarchar(200),
       url_state int,
       statetime datetime
       );

CREATE TABLE BUser (
       userId int,
       username nvarchar(200),
       created datetime,
       chatId nvarchar(200)
       );

CREATE TABLE UrlList (
       site_url nvarchar(200),
       state int,
       userId int
       );