package com.autel.cloud.pile.base.infrastructure.feign.ocpi;
/**************************************************************************
 *                                                                        *
 * Copyright (c) 2022 AUTEL Company                                       *
 * *****有限公司版权所有                                                     *
 *                                                                        *
 * PROPRIETARY RIGHTS of XXX Company are involved in the                  *
 * subject matter of this material. All manufacturing, reproduction, use, *
 * and sales rights pertaining to this subject matter are governed by the *
 * license agreement. The recipient of this software implicitly accepts   *
 * the terms of the license.                                              *
 * 本软件文档资料是XXXX有限公司的资产，任何人士阅读和使用本资料必须获得相应的书面授权，  *
 * 承担保密责任和接受相应的法律约束。                                           *
 *                                                                        *
 *************************************************************************/

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 工程包名:   com.energy.cloud
 * 项目名称:   energy-cloud-ocpi
 * 创建描述:   .
 * .
 * Creator:  LeonZhang
 * Create_Date: 2022/02/15
 * Updater:     .
 * Update_Date: .
 * Update_Desc: .
 * Class_Name: AutelCloudEnergyOcpiVersionResponse.class, (Follow OCPI v2.1.1-d2)
 * Class_Desc: .
 *
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OcpiCloudResponse<T> implements Serializable {
    private static final long serialVersionUID = -6848277837157911374L;
    /**
     * 结果执行状态码
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @JSONField(name = "status_code")
    private int status_code;

    /**
     * 结果执行状态描述
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @JSONField(name = "status_message")
    private String status_message;

    /**
     * 结果执行返回对象实体
     */
    @JSONField(name = "data")
    private T data;

    /**
     * 结果执行返回对象实体
     */
    @JSONField(name = "timestamp")
    private String timestamp;
}

