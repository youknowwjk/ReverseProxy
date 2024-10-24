package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;

/**
 * @ClassName TestService
 * @Author A22121
 * @Description
 * @Date 2022/6/29 22:53
 * @Version 0.0.1-SNAPSHOT
 */
public interface TestService {
    Result<String> testException();
}
