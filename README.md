# 关于这项服务的使用说明
这是一个获取SAP的ALV报表、底表和RFC数据的HTTP接口服务。  
  
它以ICF 服务的形式提供此接口，在此系统中，此服务已分配给 ICF 服务[/sap/bc/zsicf_bi](http://vhcala4hci:50000/sap/bc/zsicf_bi "调用地址")。  

## 权限检查

该服务包含一个对名为 ZBI_AUTH、ZBI_AUTH_F 的自定义授权对象的AUTHORITY_CHECK调用，用于验证用户是否可以访问事务码、底表或RFC,需要为每个访问该服务的用户都创建一个单独的角色，其中只有允许他访问的功能。

**权限对象**：

![pF2QXjg.jpg](https://s21.ax1x.com/2024/03/16/pF2QXjg.jpg)

**权限角色**：

![pF2lLI1.jpg](https://s21.ax1x.com/2024/03/16/pF2lLI1.jpg)

该服务提供了五类方法：获取ALV报表选择屏幕参数、获取ALV报表数据、获取底表数据、获取RFC出入参和调用RFC，具体请求示例请查看下面的详细介绍。
## 获取ALV报表选择屏幕参数
cURL请求：

    curl --location 'http://vhcala4hci:50000/sap/bc/zsicf_bi?sap-client=01&tcode=[事务码]' \
	--header 'Content-Type: application/x-www-form-urlencoded' \
	--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \
	--data-urlencode 'action=rsparams'
返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"rsparams": [
			{
				"selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",
				"kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",
				"sign": "ABAP/4: ID: I/E (包括/不包括值)",
				"option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",
				"low": "ABAP：选择值（LOW 值，外部格式）",
				"high": "ABAP：选择值（HIGH 值，外部格式）"
			}
		]
	}

## 获取ALV报表数据
cURL请求：

    curl --location 'http://vhcala4hci:50000/sap/bc/zsicf_bi?sap-client=01&tcode=[事务码]' \
	--header 'Content-Type: application/json' \
	--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \
	--data '[
		{
			"selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",
			"kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",
			"sign": "ABAP/4: ID: I/E (包括/不包括值)",
			"option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",
			"low": "ABAP：选择值（LOW 值，外部格式）",
			"high": "ABAP：选择值（HIGH 值，外部格式）"
		}
	]'

返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"mapping": [
			{
				"fieldname": "字段名",
				"scrtext_s": "短字段描述",
				"scrtext_m": "中字段描述",
				"scrtext_l": "长字段描述"
			}
		],
		"data": [
			{
				"字段名": "字段值"
			}
		]
	}

## 获取底表数据
cURL请求：

    curl --location 'http://vhcala4hci:50000/sap/bc/zsicf_bi?sap-client=01&tabname=[透明表表名]' \
	--header 'Content-Type: application/json' \
	--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \
	--data '{"wherestr":"SQL查询条件，不用加where"}'

返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"mapping": [
			{
				"fieldname": "字段名",
				"scrtext_s": "短字段描述",
				"scrtext_m": "中字段描述",
				"scrtext_l": "长字段描述"
			}
		],
		"data": [
			{
				"字段名": "字段值"
			}
		]
	}
## 获取RFC出入参
cURL请求：

    curl --location 'http://vhcala4hci:50000/sap/bc/zsicf_bi?sap-client=01&funcname=[RFC名]' \
	--header 'Content-Type: application/x-www-form-urlencoded' \
	--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \
	--data-urlencode 'action=interface'
返回消息结构：

    {
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"interface": {
			"I": {"导入参数名称":"导入参数JSON结构体"},
			"E": {"导出参数名称":"导出参数JSON结构体"},
			"C": {"更改参数名称":"更改参数JSON结构体"},
			"T": {"表参数名称":"表参数JSON结构体"}
		}
	}
## 调用RFC
cURL请求：

    curl --location 'http://vhcala4hci:50000/sap/bc/zsicf_bi?sap-client=01&funcname=[RFC名]' \
	--header 'Content-Type: application/json' \
	--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \
	--data '{
		"I": {
			"导入参数名称": "导入参数JSON结构体"
		},
		"E": {
			"导出参数名称": "导出参数JSON结构体"
		},
		"C": {
			"更改参数名称": "更改参数JSON结构体"
		},
		"T": {
			"表参数名称": "表参数JSON结构体"
		}
	}'
返回消息结构：

    {
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"interface": {
			"I": {"导入参数名称":"导入参数JSON结构体"},
			"E": {"导出参数名称":"导出参数JSON结构体"},
			"C": {"更改参数名称":"更改参数JSON结构体"},
			"T": {"表参数名称":"表参数JSON结构体"}
		}
	}
## 后续计划
1. ~~取底表数据添加OPEN SQL查询条件~~ 202405
2. ~~扩展下支持通过http调用RFC接口（完善[cesar-sap/abap_fm_json](https://github.com/cesar-sap/abap_fm_json "")不支持全部参数的缺陷）~~ 202409
3. 想得到再说吧  
  
## 联系我
邮箱：[weikj@foxmail.com](mailto:weikj@foxmail.com "kkw")
