;;; init-db-client.el --- summary -*- lexical-binding: t -*-

(setup clutch
	(:url "https://github.com/LuciusChen/clutch")
	(:doc "数据库配置通过custom.el配置，示例："
				#|
				(custom-set-variables
				 '(clutch-connection-alist
					 '(("my-oracle" . (:backend oracle :host "oracle" :port 1521 :user
		  																"system" :password "123456" :sid "XE")))))
				|#)
	(:option* clutch-connect-timeout-seconds 3
						clutch-read-idle-timeout-seconds 10
						clutch-query-timeout-seconds 5
						clutch-jdbc-rpc-timeout-seconds 5))

(setup clutch-db-jdbc
	(:doc "
Oracle, SQL Server, Snowflake, Redshift
上述数据库通过JDBC连接，需要安装agent以及相应驱动")
	(:load-after clutch)
	(:when-loaded
		(clutch-jdbc-ensure-agent)
		(clutch-jdbc-install-driver 'oracle-11)))


(provide 'init-db-client)

;;; init-db-client.el ends here
