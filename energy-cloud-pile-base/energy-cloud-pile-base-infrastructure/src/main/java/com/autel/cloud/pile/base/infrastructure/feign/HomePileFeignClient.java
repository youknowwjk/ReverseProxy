package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.britishAct.SecurityEventDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.MasterSlaveRelationDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.UpdateCardDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.britishAct.SecurityEventVO;
import com.autel.cloud.pile.bill.vo.UserChargePile;
import com.autel.cloud.pile.bill.vo.UserChargePileVO;
import com.autel.cloud.pile.bill.vo.UserSharePileJoinListVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import java.util.List;
import java.util.Set;

/**
 * Author:   A19011
 * Description: HomePileFeignClient
 * Date:     2022/5/14 16:25
 *
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(value = "home-pile", contextId = "pile-base-home")
public interface HomePileFeignClient {

    @GetMapping("/device/pile/queryByPileSn")
    @ApiOperation(value = "根据桩SN查询桩")
    Result<UserChargePile> queryByPileSn(@RequestParam("pileSn") String pileSn);

    @PostMapping("/device/pile/batchQueryCardList")
    @ApiOperation("批量查询卡列表")
    Result<List<SimpleChargeCardVO>> batchQueryCardList(@RequestBody List<String> cardNumberList, @RequestParam("userId") String userId);

    @PostMapping("/device/pile/unbindCard")
    @ApiOperation(value = "解绑卡家桩", notes = "解绑卡家桩")
    Result<Boolean> unbindCard(@RequestBody UnbindCardDTO unbindCardDTO);

    @PostMapping("/update/card/name")
    @ApiOperation(value = "更改卡名", notes = "更改卡名")
    Result<Boolean> updateCardName(@Valid @RequestBody UpdateCardDTO updateCardDTO);

    @ApiOperation(value = "同步卡名", notes = "同步卡名")
    @PutMapping("/device/pile/sync/card/name")
    Result<Boolean> syncCardName(@RequestParam("userId") Long userId, @RequestParam("name") String name, @RequestParam("cardNumber") String cardNumber);

    @GetMapping("/device/pile/getSameNameCard")
    @ApiOperation(value = "查询相同卡名称的数量")
    Result<Integer> getSameNameCard(@RequestParam("userId") Long userId, @RequestParam("name") String name, @RequestParam("cardNumber") String cardNumber);

    @GetMapping("/device/pile/getCardNumberListByUser")
    @ApiOperation(value = "获取用户卡数量")
    Result<List<String>> getCardNumberListByUser(@RequestParam("userId") String userId);


    @GetMapping("/device/pile/getCardNameByCardNumber")
    @ApiOperation(value = "查询卡号的名称")
    Result<String> getCardNameByCardNumber(@RequestParam("cardNumber") String cardNumber, @RequestParam("userId") String userId);

    @GetMapping("/device/pile/personal")
    @ApiOperation(value = "家桩列表", notes = "家桩列表")
    Result<Page<PersonalPileGunVO>> personal(
            @RequestHeader(value = "accept-language") String language,
            @RequestParam(value = "sn", required = false) String sn
            , @RequestParam(value = "page", required = true) @Min(value = 1, message = "页码必须大于等于1") int page
            , @RequestParam(value = "pageSize", required = true) @Min(value = 1, message = "每页数量必须大于等于1") @Max(value = 50, message = "每页数据量必须小于等于50") int pageSize);


    @GetMapping("/device/pile/queryBindByUser")
    @ApiOperation(value = "查询家桩是否被用户绑定", notes = "查询家桩是否被用户绑定")
    Result<Boolean> queryBindByUser(@RequestParam(value = "sn") String sn, @RequestParam(value = "userId") Long userId);

    @GetMapping("/device/pile/queryBind")
    Result<Boolean> queryBind(@RequestParam(value = "sn", required = true) String sn);

    @GetMapping("/device/pile/userPile/{sn}")
    @ApiOperation(value = "获取userPile", notes = "获取userPile")
    Result<UserChargePileVO> getUserPile(@PathVariable("sn") String sn);

    @ApiOperation(value = "当前用户作为被共享人的记录", notes = "当前用户作为被共享人的记录")
    @GetMapping("/family/share/listAsFamilyUserId")
    public Result<List<UserSharePileListVO>> listAsFamilyUserId();

    @ApiOperation(value = "桩已加入家庭成员", notes = "桩已加入家庭成员")
    @GetMapping("/family/share/listForJoin")
    Result<List<UserSharePileJoinListVO>> listForJoin(@RequestParam(value = "sn") String sn);

    @ApiOperation(value = "查询用户所属桩主ID")
    @GetMapping("/family/share/getOwnerUserIds")
    Result<Set<Long>> getOwnerUserIds(@RequestParam("userId") Long userId);


    @PostMapping("/device/pile/homePileMapCard")
    @ApiOperation("家桩地图卡片详情")
    Result<HomePileVO> getHomePileMapCard(@RequestBody HomePileMapCardDTO homePileMapCardDTO);

    @GetMapping("/device/pile/getUserIdBySN")
    @ApiModelProperty(value = "根据桩SN查询userId")
    Result<UserChargePileVO> getUserIdBySN(@RequestParam("pileSn") String pileSn);

    @PostMapping(value = "/self-adaption-lb/profile/group")
    @ApiOperation(value = "给电表使用 没有用户id", notes = "给电表使用 没有用户id")
    Result<PowerLoadBalanceVO> groupProfile(@RequestBody GroupQueryDTO groupQuery);

    @PostMapping(value = "/self-adaption-lb/update/traffic")
    @ApiOperation(value = "更新自适应功率负载均衡流量", notes = "更新自适应功率负载均衡流量")
    Result<PowerLoadBalanceVO> updateTraffic(@RequestBody UploadMeterDTO meterDTO);

    @PostMapping(value = "/self-adaption-lb/findList")
    @ApiOperation(value = "通过SN查询列表", notes = "通过SN查询列表")
    Result<List<PowerLoadBalanceVO>> findList(@RequestBody List<String> pileSnList);

    @GetMapping("/device/pile/securityEventList")
    @ApiOperation(value = "分页查询安全事件列表")
    Result<Page<SecurityEventVO>> getSecurityEventList(@RequestParam(value = "page", required = true) @Min(value = 1, message = "页码必须大于等于1") int page,
                                                       @RequestParam(value = "pageSize", required = true) @Min(value = 1, message = "每页数量必须大于等于1") int pageSize,
                                                       @RequestParam(value = "sn", required = true) String sn,
                                                       @RequestParam(value = "timezone", required = true) String timezone);


    @GetMapping("/group/pile/judgeMasterOrSlave")
    @ApiOperation(value = "查询桩是主桩还是从桩")
    Result<List<MasterSlaveRelationDTO>> judgeMasterOrSlave(@RequestParam("snList") List<String> snList);

    @PostMapping("/device/pile/snList")
    @ApiOperation(value = "根据一组序列号查询充电桩列表信息")
    Result<List<DataServiceChargePile>> queryPileList(@RequestBody @ApiParam(value = "sn列表") List<String> snList);

    @PostMapping("/device/pile/getSecurityEventListForWeb")
    @ApiOperation(value = "分页查询安全事件列表(Web端使用)")
    Result<Page<SecurityEventVO>> getSecurityEventListForWeb(@RequestBody SecurityEventDTO securityEventDTO);
}
