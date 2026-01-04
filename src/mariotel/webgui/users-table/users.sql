-- ---
-- This file is part of Mariotel
-- Copyright (C) 2020  Jean-Vincent Loddo
-- Copyright (C) 2020  Université Sorbonne Paris Nord
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- ---

-- Usage:
-- sudo mysql --defaults-file=/etc/mysql/debian.cnf <users.sql

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+02:00";
SET NAMES utf8;

--
CREATE DATABASE IF NOT EXISTS mariotel;
USE mariotel;

--
-- Table structure for `users`
--
CREATE TABLE IF NOT EXISTS `users` (
  `id`         int(11)      NOT NULL AUTO_INCREMENT,
  `username`   varchar(50)  NOT NULL,
  `password`   varchar(255) NOT NULL,
  `email`      varchar(255) DEFAULT NULL,
  `created_by` varchar(50)  DEFAULT NULL,
  `created_at` datetime     DEFAULT CURRENT_TIMESTAMP,
  `admin_level` int(1)      NOT NULL DEFAULT 0,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

--
-- Table structure for `sessions`
-- status may be 'planned' or 'running' or 'finished' or 'anomaly'
-- duration is in minutes
--
CREATE TABLE IF NOT EXISTS `sessions` (
  `id`           int(11)      NOT NULL AUTO_INCREMENT,
  `starts`       datetime     NOT NULL,
  `finish`       datetime     NOT NULL,
  `duration`     int          NOT NULL,
  `link`         varchar(255) NOT NULL,
  `status`       varchar(50)  NOT NULL,
  `username`     varchar(50)  NOT NULL,
  `email`        varchar(255) DEFAULT NULL,
  `student_nb`   int          NOT NULL,
  `student_list` text         DEFAULT NULL,
  `first_port`   int          DEFAULT NULL,
  `created_at`   datetime     DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `link` (`link`),
  UNIQUE KEY `username_starts` (`username`,`starts`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

--
-- Table structure for `workstations`
-- workstation's indexes start from 1
-- remote_socket is something like "109.13.134.196:57546"
CREATE TABLE IF NOT EXISTS `workstations` (
  `id`            int(11)      NOT NULL AUTO_INCREMENT,
  `link`          varchar(255) NOT NULL,
  `workstation`   int          NOT NULL,
  `allowed_user`  varchar(50)  NOT NULL,
  `remote_socket` varchar(50)  NOT NULL,
  `created_at`    datetime     DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `link_workstation`  (`link`,`workstation`),
  UNIQUE KEY `link_allowed_user` (`link`,`allowed_user`),
  UNIQUE KEY `link_workstation_allowed_user` (`link`,`workstation`,`allowed_user`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

--
-- mariotel grants
--
CREATE USER IF NOT EXISTS 'mariotel'@'localhost' IDENTIFIED BY 'MariotelMysql2020!!';
GRANT ALL PRIVILEGES ON *.* TO 'mariotel'@'localhost';
FLUSH PRIVILEGES;

--- Insert the "admin" user (user level 2):
INSERT INTO users (username, password, admin_level) VALUES ('admin', '$2y$10$Dbl9BuSFnx9x.xfSjriLreLzN713qlRqQ2qx1Kc7oSRZU3I2CjdPu', 2);


