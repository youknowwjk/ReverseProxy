package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.SubscribeImportAutoBindService;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * @description 历史商家桩信息导入
 * @auther A23204
 * @datetime 2023/8/23 15:59
 */

@RestController
@RequestMapping("/subscribeImport")
@Slf4j
@Validated
@Api(tags = "历史商家桩导入自动绑定")
public class SubscribeImportAutoBindController {

    @Autowired
    private SubscribeImportAutoBindService subscribeImportAutoBindService;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    /**
     * description: importSellerPileSns excel导入
     * version: 1.0
     * date: 2023/8/23 16:11 
     * author: A23204 
     * 
     * @param 
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map<java.lang.String,java.util.List<java.lang.String>>>
     */

    @ApiOperation(value = "历史商家桩需要自动的桩信息导入")
    @PostMapping("/importExcel")
    public Result<SubscribeImportResp> importSellerPileSns(@RequestParam("multipartFile") MultipartFile multipartFile) {
        // 该接口需要支持幂等. 先判断是已经绑定，已经绑定的跳过。
        log.info("--->>> importSellerPileSns start.");
        return subscribeImportAutoBindService.startImport(multipartFile);

    }

    @ApiOperation(value = "PO单历史商家手动绑定接口")
    @PostMapping("/bindManual")
    public Result<List<SubscribeBindManualDto>> bindLicenceManual(@RequestBody SubscribeBindManualReq subscribeBindManualReq) {
        log.info("--->>> bindLicenceManual start.");
        return subscribeImportAutoBindService.bindLicenceManual(subscribeBindManualReq);
    }

    /**
     * description: queryFailed 从缓存查询失败的商家桩信息
     * version: 1.0
     * date: 2023/8/23 16:40 
     * author: A23204 
     * 
     * @param 
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map<java.lang.String,java.util.List<java.lang.String>>>
     */
    @ApiOperation(value = "查询缓存中失败的桩")
    @GetMapping("/importFailed")
    public Result<List<SubscribeImportFailed>> queryImportFailedSns () {
        log.info("--->>> queryImportFailedSns start.");
        return subscribeImportAutoBindService.queryImportFailedSns();
    }

    /** 根据商户id，查询未使用licence明细信息列表
     * @return
     */
    @ApiOperation(value = "根据商户id，查询licence明细信息列表, 入参： sellerId")
    @GetMapping("/licence/queryLicence")
    Result<List<UnUsedLicenceInfoDto>> queryLicence(@RequestParam("sellerId") String sellerId) {
        return subscribePileRightsService.queryLicence(sellerId);
    }


    /**
     * description: defaultSellerLicenceImport excel导入
     * version: 1.0
     * date: 2023/8/23 16:11
     * author: A23204
     *
     * @param
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map<java.lang.String,java.util.List<java.lang.String>>>
     */

    @ApiOperation(value = "默认初始化一个月的桩套餐重新")
    @PostMapping("/defaultSellerLicenceImport")
    public Result<SubscribeImportResp> defaultSellerLicenceImport(@RequestParam("multipartFile") MultipartFile multipartFile) {
        // 该接口需要支持幂等. 先判断是已经绑定，已经绑定的跳过。
        log.info("--->>> defaultSellerLicenceImport start.");
        return subscribeImportAutoBindService.defaultSellerLicenceImport(multipartFile);

    }

    @ApiOperation(value = "默认初始化一个月的桩套餐删除，指定删除。")
    @PostMapping("/deleteDefaultOneMonth")
    public Result<Boolean> deleteDefaultOneMonth(@RequestBody List<String> pileSnList) {
        // 该接口需要支持幂等. 先判断是已经绑定，已经绑定的跳过。
        log.info("--->>> deleteDefaultOneMonth start.");
        return subscribeImportAutoBindService.deleteDefaultOneMonth(pileSnList);

    }




}
