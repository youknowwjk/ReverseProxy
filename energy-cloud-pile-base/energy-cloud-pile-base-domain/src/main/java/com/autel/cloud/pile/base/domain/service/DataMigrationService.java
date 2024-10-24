package com.autel.cloud.pile.base.domain.service;

import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;

/**
 * @ClassName DataMigrationService
 * @Author A22121
 * @Description
 * @Date 2022/6/8 15:31
 * @Version 0.0.1-SNAPSHOT
 */
public interface DataMigrationService {
    /**
     * 触发数据迁移
     *
     * @return
     */
    Result<JSONObject> trigger();

}
