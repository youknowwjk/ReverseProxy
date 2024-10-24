package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.ChargeCardService;
import com.autel.cloud.pile.base.domain.service.TbVirtualCardNfcService;
import com.autel.cloud.pile.base.dto.ChargeCardDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.vo.*;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.List;

/**
 * @Author A22282
 * @Date 2022/5/9 8:53
 */
@RestController
@RequestMapping(value = "/card")
@Api(tags = "充电卡,C端,道通APP用户相关")
@Validated
public class ChargeCardConsumerController {

    @Autowired
    private ChargeCardService chargeCardService;

    @Resource
    private TbVirtualCardNfcService tbVirtualCardNfcService;

    @Log(title = "添加充电卡(充电卡,C端,道通APP用户相关)", businessType = BusinessType.INSERT,
            code = "50201413")
    @PostMapping("/addCard")
    @ApiOperation(value = "添加充电卡")
    public Result<Boolean> addCard(@RequestBody @Validated(value = ChargeCardDTO.Default.class) ChargeCardDTO chargeCardDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        chargeCardDTO.setUserId(jwtInfo.getId());
        return chargeCardService.addCard(chargeCardDTO);
    }

    @Log(title = "添加家用卡自动添加为公用卡(充电卡,C端,道通APP用户相关)", businessType = BusinessType.INSERT,
            code = "50201413")
    @PostMapping("/bindCard")
    @ApiOperation(value = "添加家用卡自动添加为公用卡")
    public Result<Boolean> bindCard(@RequestBody @Validated(value = ChargeCardDTO.Default.class) ChargeCardDTO chargeCardDTO) {
        return chargeCardService.bindCard(chargeCardDTO);
    }

    @Log(title = "更新充电卡(充电卡,C端,道通APP用户相关)", businessType = BusinessType.UPDATE,
            code = "50201414")
    @PostMapping("/updateCard")
    @ApiOperation(value = "更新充电卡")
    public Result<Boolean> updateCard(@RequestBody @Validated(value = ChargeCardDTO.Default.class) ChargeCardDTO chargeCardDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        chargeCardDTO.setUserId(jwtInfo.getId());
        return chargeCardService.updateCard(chargeCardDTO);
    }

    @Log(title = "移除充电卡(充电卡,C端,道通APP用户相关)", businessType =
            BusinessType.DELETE, code = "50201415")
    @PostMapping("/removeCard")
    @ApiOperation(value = "移除充电卡")
    public Result<Boolean> removeCard(@RequestBody @Validated(value = ChargeCardDTO.Default.class) ChargeCardDTO chargeCardDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        chargeCardDTO.setUserId(jwtInfo.getId());
        return chargeCardService.removeCard(chargeCardDTO);
    }


    @Log(title = "网络(充电卡,C端,道通APP用户相关)", businessType =
            BusinessType.DELETE, code = "50201415")
    @PostMapping("/removeCardByNetWork")
    @ApiOperation(value = "网络移除充电卡")
    public Result<Boolean> removeCardByNetWork(@RequestBody @Validated(value = ChargeCardDTO.Default.class) ChargeCardDTO chargeCardDTO) {
        return chargeCardService.removeCard(chargeCardDTO);
    }

    @PostMapping("/cardList")
    @ApiOperation(value = "查询卡列表")
    public Result<List<ChargeCardVO>> cardList(@RequestBody ChargeCardDTO chargeCardDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        chargeCardDTO.setUserId(jwtInfo.getId());
        return chargeCardService.cardList(chargeCardDTO);
    }

    @GetMapping("/validateCard")
    @ApiOperation(value = "刷卡充电，充电卡校验")
    public Result<String> validCardNo(@RequestParam("cardNumber") String cardNumber,
                                      @RequestParam(value = "locationId",required = false)  String locationId,
                                      @RequestParam(value = "evseId",required = false) String evseId,
                                      @RequestParam(value = "operatorId",required = false) Long operatorId) {
        return Result.ofSucceed(chargeCardService.validCardNo(cardNumber, locationId, evseId,operatorId));
    }

    @GetMapping("/findCard/{cardNumber}")
    @ApiOperation(value = "根据卡号查询详情")
    public Result<ChargeCardInfoVO> findCard(@PathVariable("cardNumber") String cardNumber,@RequestParam(value = "operatorId",required = false) Long operatorId) {
        return chargeCardService.findCard(cardNumber,operatorId);
    }

    @PostMapping("/batchQueryCardList")
    @ApiOperation("批量查询卡列表")
    public Result<List<SimpleChargeCardVO>> batchQueryCardList(@RequestBody List<String> cardNumberList) {
        return Result.ofSucceed(chargeCardService.batchQueryCardList(cardNumberList));
    }

    @PostMapping("/batchQueryCardListForHubject")
    @ApiOperation("批量查询卡列表-for hubject")
    public Result<Page<ChargeCardEntity>> batchQueryCardListForHubject(@RequestBody HashMap<String, Long> laseUpdate) {
        return Result.ofSucceed(chargeCardService.batchQueryCardListForHubject(laseUpdate));
    }

    @PutMapping("/syncCardName")
    @ApiOperation(value = "更新充电卡名称")
    public Result<Boolean> syncCardName(@RequestParam("userId") Long userId, @RequestParam("name") String name, @RequestParam("cardNumber") String cardNumber) {
        return Result.ofSucceed(chargeCardService.syncCardName(userId, name, cardNumber));
    }

    @GetMapping("/getSameNameCard")
    @ApiOperation(value = "查询相同卡名称的数量")
    public Result<Integer> getSameNameCard(@RequestParam("userId") Long userId, @RequestParam("name") String name, @RequestParam("cardNumber") String cardNumber) {
        return Result.ofSucceed(chargeCardService.getSameNameCard(userId, name, cardNumber));
    }

    @GetMapping("/getCardNumberListByUser")
    @ApiOperation(value = "获取用户卡号列表")
    public Result<List<String>> getCardNumberListByUser(@RequestParam("userId") String userId) {
        return Result.ofSucceed(chargeCardService.getCardNumberListByUser(userId));
    }

    @GetMapping("/getCardNameByCardNumber")
    @ApiOperation(value = "获取用户卡数量")
    public Result<String> getCardNameByCardNumber(@RequestParam("cardNumber") String cardNumber, @RequestParam("userId") String userId) {
        return Result.ofSucceed(chargeCardService.getCardNameByCardNumber(cardNumber, userId));
    }

    @GetMapping("/getNfcCardNumber")
    @ApiOperation(value = "获取NFC虚拟卡号")
    public Result<TbVirtualCardNfcVO> getNfcCardNumber() {
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String nfcTypeString = request.getHeader("X-Client");
        Integer nfcType = Integer.valueOf(nfcTypeString);
        Long userId = LoginUserHolder.getLoginUser().getId();

        return Result.ofSucceed(tbVirtualCardNfcService.getNfcCardNumber(nfcType, userId));
    }

    @GetMapping("/validateNFCCard")
    @ApiOperation(value = "NFC虚拟充电卡校验")
    public Result<Boolean> validNFCCardNo(@RequestParam("cardNumber") String cardNumber, @RequestParam("userId") String userId) {
        return Result.ofSucceed(tbVirtualCardNfcService.validNFCCardNo(cardNumber, userId));
    }

    @Log(title = "上传NFC证书文件到本地目录")
    @PostMapping("/uploadNFCCertificateModuleXls")
    @ApiOperation(value = "上传NFC证书文件到本地目录", notes = "上传NFC证书文件到本地目录")
    public Result<String> uploadNFCCertificateModuleXls(@RequestParam("file") MultipartFile file, @RequestParam("path") String path) {
        return tbVirtualCardNfcService.uploadNFCCertificateModuleXls(file, path);
    }

    @PostMapping("/getNfcCardInfoByNumber")
    @ApiOperation("根据卡号获取卡信息")
    public Result<TbVirtualCardNfcVO> getNfcCardInfoByNumber(@RequestParam(value = "cardNumber") String cardNumber){
        return tbVirtualCardNfcService.getNfcCardInfoByNumber(cardNumber);
    }

    @PostMapping("/batchQueryCardListByUserIds")
    @ApiOperation("批量查询用户名下的充电卡")
    public Result<List<SimpleChargeCardVO>> batchQueryCardListByUserIds(@RequestBody List<String> userIds) {
        return Result.ofSucceed(chargeCardService.batchQueryCardListByUserIds(userIds));
    }
}
