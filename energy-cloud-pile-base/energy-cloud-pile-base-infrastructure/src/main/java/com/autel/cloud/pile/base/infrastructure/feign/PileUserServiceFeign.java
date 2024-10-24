package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.MemberCleanDTO;
import com.autel.cloud.pile.base.dto.MemberCustomerDTO;
import com.autel.cloud.pile.base.vo.OrganizationVO;
import com.autel.cloud.pile.base.vo.SellerInfoVO;
import com.autel.cloud.pile.base.vo.UserInfoVO;
import com.autel.cloud.pile.user.api.dto.SearchGroupDTO;
import com.autel.cloud.pile.user.api.dto.UserCompetenceDTO;
import com.autel.cloud.pile.user.api.vo.GroupVO;
import com.autel.cloud.pile.user.api.vo.OpenIdAndMemberIdVO;
import com.autel.cloud.pile.user.api.vo.UserCompetenceVO;
import com.autel.cloud.pile.user.api.vo.UserDetailVO;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @Author A22282
 * @Date 2022/5/12 14:11
 */
@FeignClient(name = "pile-user-app", contextId = "PileUserServiceFeign")
public interface PileUserServiceFeign {

    @GetMapping("/customer/{userId}")
    @ApiOperation("根据用户ID查询客户")
    Result<MemberCustomerDTO> findCustomer(@PathVariable("userId") Long userId);

    @PostMapping({"/customer/insert"})
    @ApiOperation("新增用户与客户关系")
    Result<Boolean> insert(@RequestBody MemberCustomerDTO memberCustomerDTO);

    @GetMapping("/member/user/{userId}")
    @ApiOperation("查询用户")
    Result<UserInfoVO> getUser(@RequestHeader(value = "accept-language", required = false) String language, @PathVariable("userId") @ApiParam("根据ID获取用户信息") Long userId);

    @GetMapping("/seller/detail")
    @ApiOperation(value = "获取商家详情信息")
    Result<SellerInfoVO> detail(@RequestParam("id") Long id);

    @PostMapping("/member/clean")
    @ApiOperation("用户注销")
    Result<Boolean> clean(@RequestBody @Validated MemberCleanDTO dto);

    @GetMapping("/user/getLocationIds")
    @ApiOperation(value = "根据当前用户查询场站ID")
    Result<List<Long>> getLocationIds();

    @GetMapping("/org/getOrgChildrenList")
    @ApiOperation(value = "根据组织获取子组织")
    Result<List<Long>> getOrgChildrenList(@RequestParam("orgId") Long orgId);

    @PostMapping("/org/getOrgListByIds")
    @ApiOperation(value = "根据id获取组织机构实体")
    Result<List<OrganizationVO>> getOrgListByIds(@RequestBody List<Long> orgIds);

    @GetMapping("/org/getOrgsByNameLike")
    @ApiOperation(value = "根据组织机构名称查询组织机构列表")
    Result<List<OrganizationVO>> getOrgsByNameLike(@RequestParam("orgName") String orgName);

    @GetMapping("/user/detail")
    Result<UserDetailVO> getUserDetail(@RequestParam("userId") Long userId);

    @GetMapping("/member/getIdByOpenId")
    @ApiOperation("通过openId查找客户id")
    Result<List<OpenIdAndMemberIdVO>> getIdByOpenId (@RequestParam("openIds") List<String> openIds);

    @PostMapping("/memberGroup/searchGroupByUserId")
    @ApiOperation("通过userId查找客户组")
    Result<List<GroupVO>> searchGroupByUserId(@RequestBody SearchGroupDTO searchGroupDTO);

    @GetMapping("/memberGroup/getCardByGroupId")
    @ApiOperation("通过客户组id查充电卡")
    Result<List<Long>> getCardByGroupId(@RequestParam("groupId") Long groupId);

    @PostMapping("/relation/getUserCompetence")
    @ApiOperation("查询用户权限")
    Result<UserCompetenceVO> getUserCompetence(@RequestBody UserCompetenceDTO dto);
}
