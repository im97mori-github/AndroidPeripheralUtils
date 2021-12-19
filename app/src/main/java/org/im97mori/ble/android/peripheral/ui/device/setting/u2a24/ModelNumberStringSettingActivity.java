package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.TestApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class ModelNumberStringSettingActivity extends BaseActivity {

    private ModelNumberStringSettingViewModel mViewModel;

    private CheckBox mErrorResponseCodeCheckBox;

    private TextInputLayout mModelNumberString;
    private TextInputEditText mModelNumberStringEdit;
    private TextInputLayout mErrorResponseCode;
    private TextInputEditText mErrorResponseCodeEdit;
    private TextInputLayout mResponseDelay;
    private TextInputEditText mResponseDelayEdit;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((TestApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(ModelNumberStringSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.model_number_string_setting_activity);

        mErrorResponseCodeCheckBox = findViewById(R.id.errorResponseCheckBox);

        mModelNumberString = findViewById(R.id.modelNumberString);
        mModelNumberStringEdit = (TextInputEditText) mModelNumberString.getEditText();

        mResponseDelay = findViewById(R.id.responseDelay);
        mResponseDelayEdit = (TextInputEditText) mResponseDelay.getEditText();

        mErrorResponseCode = findViewById(R.id.errorResponseCode);
        mErrorResponseCodeEdit = (TextInputEditText) mErrorResponseCode.getEditText();

        mViewModel.observeIsErrorResponse(this, aBoolean -> {
            mErrorResponseCode.setVisibility(aBoolean ? View.VISIBLE : View.GONE);
            mErrorResponseCodeCheckBox.setChecked(aBoolean);
        });
        mViewModel.observeResponseCode(this, charSequence -> distinctSetText(mErrorResponseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, charSequence -> mErrorResponseCode.setError(charSequence));
        mErrorResponseCodeCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));
        mErrorResponseCodeEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseCode(s);
            }
        });

        mViewModel.observeResponseDelay(this, charSequence -> distinctSetText(mResponseDelayEdit, charSequence));
        mViewModel.observeResponseDelayError(this, charSequence -> mResponseDelay.setError(charSequence));
        mResponseDelayEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseDelay(s);
            }
        });

        mViewModel.observeManufacturerNameString(this, charSequence -> distinctSetText(mModelNumberStringEdit, charSequence));
        mViewModel.observeManufacturerNameStringError(this, charSequence -> mModelNumberString.setError(charSequence));
        mModelNumberStringEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateManufacturerNameString(s);
            }
        });

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
