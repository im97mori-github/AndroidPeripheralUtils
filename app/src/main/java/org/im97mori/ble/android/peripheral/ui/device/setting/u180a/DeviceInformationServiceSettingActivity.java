package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.card.MaterialCardView;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.TestApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringLauncherContract;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class DeviceInformationServiceSettingActivity extends BaseActivity {

    private DeviceInformationServiceSettingViewModel mViewModel;

    final ActivityResultLauncher<String> mStartManufacturerNameStringSettingActivity = registerForActivityResult(new ManufacturerNameStringLauncherContract()
            , result -> mViewModel.setManufacturerNameStringCharacteristicDataString(result));

    final ActivityResultLauncher<String> mStartModelNumberStringSettingActivity = registerForActivityResult(new ModelNumberStringLauncherContract()
            , result -> mViewModel.setModelNumberStringCharacteristicDataString(result));

    final ActivityResultLauncher<String> mStartSystemIdSettingActivity = registerForActivityResult(new SystemIdLauncherContract()
            , result -> mViewModel.setSystemIdCharacteristicDataString(result));

    private MaterialCardView mManufacturerNameStringCardView;
    private TextView mManufacturerNameString;

    private MaterialCardView mModelNumberStringCardView;
    private TextView mModelNumberString;

    private CheckBox mSystemIdCharacteristicSupported;

    private MaterialCardView mSystemIdCardView;
    private TextView mManufacturerIdentifier;
    private TextView mOrganizationallyUniqueIdentifier;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((TestApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(DeviceInformationServiceSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.device_information_service_setting_activity);

        mManufacturerNameStringCardView = findViewById(R.id.manufacturerNameStringCardView);
        mManufacturerNameString = findViewById(R.id.manufacturerNameString);

        mModelNumberStringCardView = findViewById(R.id.modelNumberStringCardView);
        mModelNumberString = findViewById(R.id.modelNumberString);

        mSystemIdCardView = findViewById(R.id.systemIdCardView);
        mManufacturerIdentifier = findViewById(R.id.manufacturerIdentifier);
        mOrganizationallyUniqueIdentifier = findViewById(R.id.organizationallyUniqueIdentifier);

        mSystemIdCharacteristicSupported = findViewById(R.id.systemIdCharacteristicSupported);

        mViewModel.observeHasManufacturerNameString(this, aBoolean -> mManufacturerNameStringCardView.setChecked(aBoolean));
        mViewModel.observeHasModelNumberString(this, aBoolean -> mModelNumberStringCardView.setChecked(aBoolean));
        mViewModel.observeHasSystemId(this, aBoolean -> mSystemIdCardView.setChecked(aBoolean));
        mViewModel.observeSupportSystemId(this, aBoolean -> {
            if (aBoolean) {
                mSystemIdCardView.setVisibility(View.VISIBLE);
            } else {
                mSystemIdCardView.setChecked(false);
                mSystemIdCardView.setVisibility(View.GONE);
            }
            mSystemIdCharacteristicSupported.setChecked(aBoolean);
        });

        mViewModel.observeManufacturerNameString(this, s -> mManufacturerNameString.setText(s));
        mViewModel.observeModelNumberString(this, s -> mModelNumberString.setText(s));
        mViewModel.observeManufacturerIdentifier(this, s -> mManufacturerIdentifier.setText(s));
        mViewModel.observeOrganizationallyUniqueIdentifier(this, s -> mOrganizationallyUniqueIdentifier.setText(s));

        mSystemIdCharacteristicSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateSupportSystemId(isChecked));

        findViewById(R.id.manufacturerNameStringSettingButton).setOnClickListener(v -> mStartManufacturerNameStringSettingActivity.launch(mViewModel.getManufacturerNameStringCharacteristicDataString()));
        findViewById(R.id.modelNumberStringSettingButton).setOnClickListener(v -> mStartModelNumberStringSettingActivity.launch(mViewModel.getModelNumberStringCharacteristicDataString()));
        findViewById(R.id.systemIdSettingButton).setOnClickListener(v -> mStartSystemIdSettingActivity.launch(mViewModel.getSystemIdCharacteristicDataString()));

        MaterialToolbar bar = findViewById(R.id.topAppBar);
        bar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result = false;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(intent -> {
                                if (intent.isPresent()) {
                                    setResult(RESULT_OK, intent.get());
                                    finish();
                                }
                            }
                    ));

        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
