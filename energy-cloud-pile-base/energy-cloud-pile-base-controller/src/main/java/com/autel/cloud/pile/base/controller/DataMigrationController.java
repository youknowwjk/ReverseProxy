package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.DataMigrationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @ClassName DataMigrationController
 * @Author A22121
 * @Description
 * @Date 2022/6/8 14:53
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@Validated
@RequestMapping("/dataMigration")
@Api(value = "数据迁移", tags = "数据迁移")
public class DataMigrationController {

    @Autowired
    private DataMigrationService dataMigrationService;

    @GetMapping("/trigger")
    @ApiOperation(value = "触发数据迁移", notes = "触发数据迁移")
    public Result<JSONObject> trigger() {
        return dataMigrationService.trigger();
    }
}
