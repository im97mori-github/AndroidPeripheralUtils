package org.im97mori.ble.android.peripheral.ui.device;

import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class PeripheralViewModel extends ViewModel {

    private final DeviceRepository mDeviceRepository;

    private final Gson mGson;

    @Inject
    public PeripheralViewModel(@NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        mDeviceRepository = deviceRepository;
        mGson = gson;
    }

    @NonNull
    @MainThread
    public Single<MockData> setup(@NonNull Intent intent) {
        return mDeviceRepository
                .loadDeviceById(intent.getLongExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID))
                .subscribeOn(Schedulers.io())
                .flatMap(device -> Single.just(mGson.fromJson(device.getDeviceSetting(), MockData.class)))
                .observeOn(AndroidSchedulers.mainThread());
    }

}