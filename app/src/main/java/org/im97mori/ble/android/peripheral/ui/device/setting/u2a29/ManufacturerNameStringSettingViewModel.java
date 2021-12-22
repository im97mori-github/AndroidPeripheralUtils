package org.im97mori.ble.android.peripheral.ui.device.setting.u2a29;

import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class ManufacturerNameStringSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_MANUFACTURER_NAME_STRING = "KEY_MANUFACTURER_NAME_STRING";
    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<String> mManufacturerNameString;
    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    @Inject
    public ManufacturerNameStringSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        super(deviceRepository, gson);

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);
        mManufacturerNameString = savedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING);
        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        return Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                try {
                    mCharacteristicData = mGson.fromJson(intent.getStringExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString())
                            , CharacteristicData.class);
                } catch (JsonSyntaxException e) {
                    e.printStackTrace();
                }

                if (mCharacteristicData == null) {
                    mCharacteristicData = new CharacteristicData();
                    mCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
                    mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                    mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                }

                if (mIsErrorResponse.getValue() == null) {
                    mIsErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
                }

                if (mManufacturerNameString.getValue() == null) {
                    if (mCharacteristicData.data != null) {
                        mManufacturerNameString.postValue(new ManufacturerNameString(mCharacteristicData.data).getManufacturerName());
                    } else {
                        mManufacturerNameString.postValue(null);
                    }
                }

                if (mResponseCode.getValue() == null) {
                    mResponseCode.postValue(String.valueOf(mCharacteristicData.responseCode));
                }

                if (mResponseDelay.getValue() == null) {
                    mResponseDelay.postValue(String.valueOf(mCharacteristicData.delay));
                }

                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Initialized"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

    @MainThread
    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsErrorResponse).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameString).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameStringError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameString).observe(owner
                , s -> observer.onChanged(mDeviceRepository.getManufacturerNameStringErrorString(s)));
    }

    @MainThread
    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCodeError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner
                , s -> observer.onChanged(mDeviceRepository.getResponseCodeErrorString(s)));
    }

    @MainThread
    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner, observer);
    }

    @MainThread
    public void observeResponseDelayError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner
                , s -> observer.onChanged(mDeviceRepository.getResponseDelayErrorString(s)));
    }

    @MainThread
    public void updateIsErrorResponse(@NonNull Boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateManufacturerNameString(@NonNull String text) {
        mManufacturerNameString.setValue(text);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @NonNull
    @Override
    public Single<Intent> save() {
        return Single.<Intent>create(emitter -> {
//            emitter.onError(new RuntimeException());
            CharacteristicData characteristicData = mCharacteristicData;
            if (characteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                String manufacturerNameString = mManufacturerNameString.getValue();

                if (responseDelay != null && mDeviceRepository.getResponseDelayErrorString(responseDelay) == null) {
                    characteristicData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceRepository.getResponseCodeErrorString(responseCode) == null) {
                            characteristicData.data = null;
                            characteristicData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));
                            emitter.onSuccess(intent);
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        if (manufacturerNameString != null && mDeviceRepository.getManufacturerNameStringErrorString(manufacturerNameString) == null) {
                            characteristicData.data = new ManufacturerNameString(manufacturerNameString).getBytes();
                            characteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;

                            Intent intent = new Intent();
                            intent.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));

                            mCharacteristicData = null;
                            emitter.onSuccess(intent);
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    }
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }
}
