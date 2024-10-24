package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.pile.base.domain.service.ChargeCardService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.vo.CardOptionsPageVO;
import com.autel.cloud.pile.base.vo.ChargeCardBusinessVO;
import com.autel.cloud.pile.base.vo.ChargeCardImportVO;
import com.autel.cloud.pile.base.vo.ChargeCardPageVO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @Author A22282
 * @Date 2022/5/29 11:50
 */
@RestController
@RequestMapping("/card")
@Api(tags = "充电卡，B端接口")
@Slf4j
@Validated
public class ChargeCardBusinessController {

    @Autowired
    private ChargeCardService chargeCardService;

    @Autowired
    private OpLocationService opLocationService;

    @Value("${autel.admin:0}")
    private Long admin;

    @OperationActionLog(action = "add",object = "chargeCard")
    @PostMapping("/add")
    @ApiOperation("新增充电卡")
    public Result<String> add(@RequestBody @Validated(ChargeCardBusinessDTO.Add.class) ChargeCardBusinessDTO cardBusinessDTO) {
        cardBusinessDTO.setOperatorId(LoginUserUtil.getSellerId());
        log.info("add,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        if (admin.equals(cardBusinessDTO.getOperatorId())){
            return chargeCardService.addForBusinessAdmin(cardBusinessDTO);
        }else {
            return chargeCardService.addForBusiness(cardBusinessDTO);
        }
    }

    @OperationActionLog(action = "edit",object = "chargeCard")
    @PostMapping("/update")
    @ApiOperation("编辑充电卡")
    public Result<Boolean> update(@RequestBody @Validated(ChargeCardBusinessDTO.Update.class) ChargeCardBusinessDTO cardBusinessDTO) {
        cardBusinessDTO.setOperatorId(LoginUserUtil.getSellerId());
        log.info("update,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        return chargeCardService.updateForBusiness(cardBusinessDTO);
    }

    @OperationActionLog(action = "query",object = "chargeCard")
    @GetMapping("/detail/{id}")
    @ApiOperation("充电卡详情")
    public Result<ChargeCardBusinessVO> detail(@PathVariable("id") Long id) {
        return chargeCardService.detail(id);
    }

    @PostMapping("/pages")
    @ApiOperation("分页查询")
    @Deprecated
    public Result<IPage<ChargeCardBusinessVO>> pages(@RequestBody ChargeCardBusinessPageDTO cardBusinessDTO) {
        cardBusinessDTO.setOperatorId(LoginUserUtil.getSellerId());
        log.info("pages,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        return chargeCardService.pagesForBusiness(cardBusinessDTO);
    }

    @PostMapping("/syncChargeTimes")
    @ApiOperation("充电次数同步")
    public Result<Boolean> syncChargeTimes(@RequestBody @Validated ChargeCardOrderDTO dto) {
        return chargeCardService.syncChargeTimes(dto);
    }

    @OperationActionLog(action = "query",object = "chargeCard")
    @PostMapping("/pagesV2")
    @ApiOperation("分页查询")
    public Result<IPage<ChargeCardPageVO>> pagesV2(@RequestBody ChargeCardPageDTO chargeCardPageDTO) {
        chargeCardPageDTO.setOperatorId(LoginUserUtil.getSellerId());
        log.info("pagesV2,chargeCardPageDTO={}", JSON.toJSONString(chargeCardPageDTO));
        return chargeCardService.pagesV2(chargeCardPageDTO);
    }

    @OperationActionLog(action = "query",object = "chargeCard")
    @PostMapping("/cardOptionPageList")
    @ApiOperation("分页查询本地白名单可选卡列表")
    public Result<IPage<CardOptionsPageVO>> cardOptionPageList(@RequestBody CardOptionsDTO cardOptionsDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        cardOptionsDTO.setOperatorId(payload.getSellerId());
        log.info("cardOptionPageList chargeCardPageDTO : {}", JSON.toJSONString(cardOptionsDTO));
        return chargeCardService.cardOptionPageList(cardOptionsDTO);
    }

    @GetMapping("/findList")
    @ApiOperation("充电卡列表，过滤已添加车主")
    public Result<List<ChargeCardBusinessVO>> findList(@RequestParam(value = "driverId", required = false) @ApiParam("车主ID") Long driverId) {
        Long sellerId = LoginUserUtil.getSellerId();
        return Result.ofSucceed(this.chargeCardService.findList(sellerId, driverId));
    }

    @GetMapping("/searchBindCard")
    @ApiOperation("查找已绑定客户组的充电卡")
    public Result<List<Long>> searchBindCard(@RequestParam("groupId") Long groupId) {
        return chargeCardService.searchBindCard(groupId);
    }
    @OperationActionLog(action = "delete",object = "chargeCard")
    @PostMapping("/delete/{id}")
    @ApiOperation("移除充电卡")
    public Result<Boolean> delete(@PathVariable("id") Long id) {
        log.info("delete,id={}", id);
        return chargeCardService.deleteForBusiness(id);
    }

    @OperationActionLog(action = "delete",object = "chargeCard")
    @PostMapping("/batchRemovalOfChargingCards")
    @ApiOperation("批量移除充电卡")
    public Result<Boolean> batchRemovalOfChargingCards(@RequestBody List<Long> ids) {
        log.info("batchRemovalOfChargingCards,ids={}", ids);
        return chargeCardService.batchRemovalOfChargingCards(ids);
    }

    @OperationActionLog(action = "edit",object = "chargeCard")
    @PostMapping("/enableDisableChargingCard")
    @ApiOperation("启用/禁用充电卡")
    public Result<Boolean> enableDisableChargingCard(@RequestBody EnableDisableChargingCardDTO enableDisableChargingCardDTO) {
        log.info("enableDisableChargingCard, enableDisableChargingCardDTO={}",enableDisableChargingCardDTO);
        return chargeCardService.enableDisableChargingCard(enableDisableChargingCardDTO);
    }

    @OperationActionLog(action = "query",object = "chargeCard")
    @PostMapping("/getDisplaySetting")
    @ApiOperation(value = "显示设置-获取", notes = "显示设置-获取")
    public Result<Map<String, Object>> getDisplaySetting() {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        return Result.ofSucceed(chargeCardService.getDisplaySetting(payload.getSellerId(), payload.getUserId()));
    }

    @OperationActionLog(action = "edit",object = "chargeCard")
    @PostMapping("/setDisplaySetting")
    @ApiOperation(value = "显示设置-保存", notes = "显示设置-保存")
    public Result<Map<String, Object>> setDisplaySetting(@RequestBody ChargeCardDisPlayV2DTO ChargeCardDisPlayV2DTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        ChargeCardDisPlayV2DTO.setOperatorId(payload.getSellerId());
        ChargeCardDisPlayV2DTO.setUserId(payload.getUserId());
        return Result.ofSucceed(chargeCardService.setDisplaySetting(ChargeCardDisPlayV2DTO));
    }

    @PostMapping("/exchangeDisplayColumn")
    @ApiOperation(value = "显示设置-获取", notes = "显示设置-获取")
    public Result<Map<String, Object>> exchangeDisplayColumn(@RequestBody List<String> dataIndexList) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        return Result.ofSucceed(chargeCardService.exchangeDisplayColumn(payload.getSellerId(), payload.getUserId(), dataIndexList));
    }

    @DeleteMapping("/deleteUserData")
    @ApiOperation("清除用户绑定的充电卡")
    Result<Boolean> deleteUserData(@RequestParam("userId") Long userId) {
        return chargeCardService.deleteUserData(userId);
    }

    @OperationActionLog(action = "export",object = "chargeCard")
    @PostMapping("/download")
    @ApiOperation("下载批量导入充电卡模板")
    public void download(HttpServletRequest request, HttpServletResponse response) {
        Long sellerId = LoginUserUtil.getSellerId();
        chargeCardService.download(request, response, sellerId);
    }

    @OperationActionLog(action = "import",object = "chargeCard")
    @PostMapping("/import")
    @ApiOperation("批量导入充电卡")
    @ApiResponses(
            {
                    @ApiResponse(code = 50201470, message = "请填写必填信息"),
                    @ApiResponse(code = 50201471, message = "文档解析失败，请重试"),
                    @ApiResponse(code = 50201472, message = "部分项目导入失败，根据返回的code，调用接口errorDetail/{code}点此下载添加详情"),
            }
    )
    public Result<ChargeCardImportVO> imports(HttpServletRequest request, @RequestParam("file") MultipartFile file) {
        Long sellerId = LoginUserUtil.getSellerId();
        return Result.ofSucceed(this.chargeCardService.imports(request, file, sellerId));
    }

    @OperationActionLog(action = "query",object = "chargeCard")
    @PostMapping("/errorDetail/{code}")
    @ApiOperation("点此下载添加详情")
    public void errorDetail(HttpServletRequest request, HttpServletResponse response,@PathVariable ("code") Long code) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        chargeCardService.errorDetail(request, response, payload.getSellerId(),code);
    }

    @PostMapping("/getLocations")
    @ApiOperation("根据当前商家获取场站")
    public Result<List<OpLocationMenuDTO>> getLocations() {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        Long sellerId = payload.getSellerId();
        if (sellerId.equals(admin)) {
            List<OpLocationMenuDTO> stationMenu = opLocationService.getStationMenu();
            return Result.ofSucceed(stationMenu);
        }
        List<OpLocationDTO> data = (List<OpLocationDTO>) opLocationService.getLocationBySellerId().getData();
        List<OpLocationMenuDTO> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(data)) {
            data.forEach(e -> {
                OpLocationMenuDTO entity = new OpLocationMenuDTO();
                BeanUtils.copyProperties(e, entity);
                result.add(entity);
            });
        }
        return Result.ofSucceed(result);
    }

    @PostMapping("/syncData")
    @ApiOperation("同步充电卡数据")
    Result<Boolean> syncCard(@RequestBody List<String> cardNumbers){
        return chargeCardService.syncCard(cardNumbers);
    }

    @PostMapping("/getCardInfoByNumber")
    @ApiOperation("根据卡号获取卡信息")
    Result<ChargeCardPageVO> getCardInfoByNumber(@RequestParam(value = "cardNumber") String cardNumber){
        return chargeCardService.getCardInfoByNumber(cardNumber);
    }

    @PostMapping("/getCardInfoByNumberAndUser")
    @ApiOperation("根据卡号获取卡信息--上面方法存在报错，同一充电卡号多条记录")
    Result<ChargeCardPageVO> getCardInfoByNumberAndUser(@RequestParam(value = "cardNumber") String cardNumber,
                                                 @RequestParam(value = "userId",required = false) Long userId,
                                                 @RequestParam(value = "operatorId",required = false) Long operatorId){
        log.info("===getCardInfoByNumberAndUser cardNumber:{} userId:{} operatorId:{}",cardNumber,userId,operatorId);
        return chargeCardService.getCardInfoByNumberAndUser(cardNumber,userId,operatorId);
    }

    @PostMapping("/batchGetCardInfoByNumber")
    @ApiOperation("批量获取充电卡信息")
    Result<List<ChargeCardPageVO>> batchGetCardInfoByNumber(@RequestBody BatchQueryCardInfoParamDTO batchQueryCardInfoParamDTO){
        return chargeCardService.batchGetCardInfoByNumber(batchQueryCardInfoParamDTO);
    }

    @GetMapping("/getAllCardIdBySellerId")
    @ApiOperation("查询商家底下所有的充电卡号")
    public Result<List<String>> getAllCardIdBySellerId(@RequestParam("sellerId") Long sellerId) {
        return Result.ofSucceed(chargeCardService.getAllCardIdBySellerId(sellerId));
    }

    @ApiOperation("删除所有充电卡显示设置")
    @GetMapping("/deletedAllDisPlaySetting")
    public Result<Boolean> deletedAllDisPlaySetting() {
        return Result.ofSucceed(chargeCardService.deletedAllDisPlaySetting());
    }
}
