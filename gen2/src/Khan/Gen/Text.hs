{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Gen.Text
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Text where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.Attoparsec.Text  as Parse
import           Data.Char
import           Data.Foldable         (foldl')
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.ICU         (Regex)
import           Data.Text.ICU.Replace (Replace)
import qualified Data.Text.ICU.Replace as RE
import           Data.Text.Manipulate

asText :: (Text -> Text) -> String -> String
asText f = Text.unpack . f . Text.pack

stripLens :: Text -> Text
stripLens t
    | "_" `Text.isPrefixOf` t = lowerHead (dropLower t)
    | otherwise               = t

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . isUpper)

replaceAcronyms :: Text -> Text
replaceAcronyms x = foldl' (flip (uncurry RE.replaceAll)) x xs
  where
    xs :: [(Regex, Replace)]
    xs = [ ("Acl",           "ACL")
         , ("Adm([^i]|$)",   "ADM$1")
         , ("Aes",           "AES")
         , ("Api",           "API")
         , ("Ami",           "AMI")
         , ("Apns",          "APNS")
         , ("Arn",           "ARN")
         , ("Asn",           "ASN")
         , ("Aws",           "AWS")
         , ("Bcc([A-Z])",    "BCC$1")
         , ("Bgp",           "BGP")
         , ("Cc([A-Z])",     "CC$1")
         , ("Cidr",          "CIDR")
         , ("Cors",          "CORS")
         , ("Csv",           "CSV")
         , ("Cpu",           "CPU")
         , ("Db",            "DB")
         , ("Dhcp",          "DHCP")
         , ("Dns",           "DNS")
         , ("Ebs",           "EBS")
         , ("Ec2",           "EC2")
         , ("Eip",           "EIP")
         , ("Gcm",           "GCM")
         , ("Html",          "HTML")
         , ("Https",         "HTTPS")
         , ("Http([^s]|$)",  "HTTP$1")
         , ("Hsm",           "HSM")
         , ("Hvm",           "HVM")
         , ("Iam",           "IAM")
         , ("Icmp",          "ICMP")
         , ("Id$",           "ID")
         , ("Id([A-Z])",     "ID$1")
         , ("Idn",           "IDN")
         , ("Ids$",          "IDs")
         , ("Ids([A-Z])",    "IDs$1")
         , ("Iops",          "IOPS")
         , ("Ip",            "IP")
         , ("Jar",           "JAR")
         , ("Json",          "JSON")
         , ("Jvm",           "JVM")
         , ("Kms",           "KMS")
         , ("Mac([^h]|$)",   "MAC$1")
         , ("Md5",           "MD5")
         , ("Mfa",           "MFA")
         , ("Ok",            "OK")
         , ("Os",            "OS")
         , ("Php",           "PHP")
         , ("Raid",          "RAID")
         , ("Ramdisk",       "RAMDisk")
         , ("Rds",           "RDS")
         , ("Sni",           "SNI")
         , ("Sns",           "SNS")
         , ("Sriov",         "SRIOV")
         , ("Ssh",           "SSH")
         , ("Ssl",           "SSL")
         , ("Svn",           "SVN")
         , ("Tar([^g]|$)",   "TAR$1")
         , ("Tde",           "TDE")
         , ("Tcp",           "TCP")
         , ("Tgz",           "TGZ")
         , ("Tls",           "TLS")
         , ("Uri",           "URI")
         , ("Url",           "URL")
         , ("Vgw",           "VGW")
         , ("Vhd",           "VHD")
         , ("Vip",           "VIP")
         , ("Vlan",          "VLAN")
         , ("Vm([^d]|$)",    "VM$1")
         , ("Vmdk",          "VMDK")
         , ("Vpc",           "VPC")
         , ("Vpn",           "VPN")
         , ("Xml",           "XML")
         ]