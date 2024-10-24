package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.app.AppUnit;
import com.autel.cloud.pile.bill.vo.UserDetailInfo;
import com.autel.cloud.pile.bill.vo.UserSetVO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * @author A21309
 * @date 2021-07-16 14:28
 */
@FeignClient(value = "base-admin-app", contextId = "pile-base")
public interface BaseAdminClient {

    @PostMapping("/unit/getByUserId")
    Result<List<UserSetVO>> queryUserSet(@RequestBody List<String> userList);

    @GetMapping("/user/batch")
    Result<List<UserDetailInfo>> queryUserInfo(@RequestParam(value = "idList") List<Long> idList);

    @ApiOperation("获取用户设置")
    @GetMapping("/unit/getSetByUserId")
    Result<AppUnit> getSetByUserId(@RequestParam(value = "userId") String userId);

    @ApiOperation(" 获取货币code，国家缩写Map集合")
    @PostMapping("/location/getCurrencyAndAlpha2CodeMap")
    Result<Map<String, Integer>> getCurrencyAndAlpha2CodeMap(@RequestBody List<Integer> currencyCodes);

    /**
     * description: getCountryInfoByAlpha2Code 根据国家2字码，查询所属环境信息（US，EU，CA，CN）
     * version: 1.0
     * date: 2023/7/12 16:36 
     * author: A23204 
     * 
     * @param alpha2Code
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.String>
     */ 
    @GetMapping("/location/query/{alpha2Code}")
    Result<String> getCountryInfoByAlpha2Code(@PathVariable(value = "alpha2Code") String alpha2Code);

}
