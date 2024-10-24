package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.vo.Attachment;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

@FeignClient(value = "common-service")
public interface CommonServiceClient {

    @ApiOperation(value = "上传附件,增加key")
    @PostMapping(value = "/attachment/upload/{way}/{key}", consumes =  MediaType.MULTIPART_FORM_DATA_VALUE)
    @ResponseBody
    Result<Attachment> upload(@RequestPart(value = "file") MultipartFile file, @PathVariable("way") String way, @PathVariable("key") String key);
}
